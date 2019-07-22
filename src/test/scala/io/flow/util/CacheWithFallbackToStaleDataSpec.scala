package io.flow.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.concurrent.Eventually.{eventually, timeout}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class CacheWithFallbackToStaleDataSpec extends WordSpecLike with MustMatchers {
  implicit val executionContext = ExecutionContext.global

  private[this] case class TestCacheWithFallbackToStaleData[T]() extends CacheWithFallbackToStaleData[String, T] {

    private[this] val data = new ConcurrentHashMap[String, T]()
    private[this] val nextValues = new ConcurrentHashMap[String, T]()
    private val refreshes = new AtomicInteger()

    def numberRefreshes: Int = refreshes.intValue()

    @volatile
    var refreshShouldFail = false
    @volatile
    var refreshDelayMs = 0L

    override def refresh(key: String): T = {
      if (!refreshShouldFail) {
        refreshes.incrementAndGet()
        Thread.sleep(refreshDelayMs)
        data.put(
          key,
          Option(nextValues.get(key))
            .getOrElse(Option(data.get(key)).getOrElse(sys.error(s"0 - Missing test data for key[$key]")))
        )
      }

      Option(data.get(key)).getOrElse(sys.error(s"1 - Missing test data for key[$key]"))
    }

    def setNextValue(key: String, value: T): Unit = {
      nextValues.put(key, value)
      ()
    }

    def set(key: String, value: T): Unit = {
      data.put(key, value)
      ()
    }

  }

  private[this] def eventuallyInNSeconds[T](n: Int)(f: => T): T = {
    eventually(timeout(Span(n.toLong, Seconds))) {
      f
    }
  }

  "cached values are served" in {
    val cache = new TestCacheWithFallbackToStaleData[String]() {
      override val duration: FiniteDuration = FiniteDuration(1, SECONDS)
    }
    cache.set("a", "apple")

    // Test cache hit
    cache.get("a") must equal("apple")
    cache.get("a") must equal("apple")
    cache.numberRefreshes must equal(1)

    // Test cache miss and auto refresh after 1 second
    eventuallyInNSeconds(2) {
      cache.get("a") must equal("apple")
      cache.numberRefreshes must equal(2)
    }
  }

  "supports multiple keys" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.set("a", "apple")
    cache.set("p", "pear")

    cache.get("a") must equal("apple")
    cache.get("p") must equal("pear")
  }

  "failed refresh handled gracefully" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.set("a", "apple")
    cache.get("a") must equal("apple")

    cache.refreshShouldFail = true
    cache.setNextValue("a", "not apple")

    Thread.sleep(2000)

    // failing refresh should return old value
    cache.get("a") must equal("apple")
  }

  "flushed key serves stale data if refresh fails" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.set("a", "apple")
    cache.flush("a")

    cache.refreshShouldFail = true
    cache.setNextValue("a", "foo")

    // failing refresh should return old value
    cache.get("a") must equal("apple")
  }

  "flushed key is immediately refreshed" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.set("a", "apple")
    cache.setNextValue("a", "foo")
    cache.flush("a")

    // failing refresh should return old value
    cache.get("a") must equal("foo")
  }

  "flushing key triggers refresh" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.setNextValue("a", "apple")
    cache.get("a") must equal("apple")
    cache.setNextValue("a", "foo")
    cache.get("a") must not equal("foo")
    cache.flush("a")
    cache.get("a") must equal("foo")
  }

  "Caching none is cached for short period of time, some for longer" in {
    val cache = TestCacheWithFallbackToStaleData[Option[String]]()
    cache.setNextValue("a", Some("baz"))
    cache.setNextValue("b", None)

    cache.get("a") must equal(Some("baz"))
    cache.get("b") must equal(None)

    cache.setNextValue("a", Some("baz2"))
    cache.setNextValue("b", Some("foo"))

    cache.get("a") must equal(Some("baz"))
    cache.get("b") must equal(None)

    eventuallyInNSeconds(2) {
      cache.get("b") must equal(Some("foo"))
    }
    cache.get("a") must equal(Some("baz"))  // still cached
  }

  "Concurrent get should refresh only once" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.setNextValue("a", "apple")
    cache.refreshDelayMs = 500L

    val get1 = Future { cache.get("a") }
    val get2 = Future { cache.get("a") }
    Await.result(get1, 1.second) must equal("apple")
    Await.result(get2, 1.second) must equal("apple")
    cache.numberRefreshes must be(1)

    cache.flush("a")

    cache.setNextValue("a", "pear")
    val get3 = Future { cache.get("a") }
    val get4 = Future { cache.get("a") }
    Await.result(get3, 1.second) must equal("pear")
    Await.result(get4, 1.second) must equal("pear")
    cache.numberRefreshes must be(2)
  }

}
