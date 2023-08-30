package io.flow.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.concurrent.Eventually.{eventually, timeout}
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class CacheWithFallbackToStaleDataSpec extends AnyWordSpecLike with Matchers {
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
      refreshes.incrementAndGet()
      Thread.sleep(refreshDelayMs)
      if (refreshShouldFail) {
        throw new RuntimeException("boom")
      } else {
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

  private[this] def time[T](f: => T): (T, Long) = {
    val start = System.currentTimeMillis()
    val ret: T = f
    val elapsed = System.currentTimeMillis() - start
    (ret, elapsed)
  }

  "cached values are served" in {
    val cache = new TestCacheWithFallbackToStaleData[String]() {
      override def refreshInterval: FiniteDuration = FiniteDuration(1, SECONDS)
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

  "flushed key throws if refresh fails" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.set("a", "apple")
    cache.flush("a")

    cache.refreshShouldFail = true

    an[Exception] must be thrownBy cache.get("a")
  }

  "flushed key is immediately refreshed" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    cache.set("a", "apple")
    cache.setNextValue("a", "foo")
    cache.flush("a")

    // succeeding refresh should return old value
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

  "safeGet" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    an[Exception] must be thrownBy cache.get("a")

    noException must be thrownBy cache.safeGet("a")
    cache.safeGet("a").isFailure mustBe true
  }

  "getOrElse" in {
    val cache = TestCacheWithFallbackToStaleData[String]()
    an[Exception] must be thrownBy cache.get("a")

    noException must be thrownBy cache.getOrElse("a", "default")
    cache.getOrElse("a", "default") mustBe "default"

    cache.set("b", "nonDefault")
    cache.getOrElse("b", "default") mustBe "nonDefault"
  }

  "initialContents" in {
    val cache = new TestCacheWithFallbackToStaleData[String]() {
      override def initialContents(): Seq[(String, String)] = Seq("a" -> "apple", "p" -> "pear")
      refreshDelayMs = 500L
    }

    cache.set("b", "banana")

    val (v1, t1) = time(cache.get("a"))
    v1 mustBe "apple"
    t1 must be < 100L

    val (v2, t2) = time(cache.get("p"))
    v2 mustBe "pear"
    t2 must be < 100L

    val (v3, t3) = time(cache.get("b"))
    v3 mustBe "banana"
    t3 must be >= 500L
  }

  "Initial load should block, refresh should happen in the background" in {
    val cache = new TestCacheWithFallbackToStaleData[String]() {
      override def refreshInterval: FiniteDuration = FiniteDuration(1, SECONDS)
      refreshDelayMs = 500L
    }

    cache.set("a", "apple")

    val (v1, t1) = time(cache.get("a"))
    v1 mustBe "apple"
    t1 must be >= 500L

    Thread.sleep(1000L)
    cache.set("a", "apricot")

    val (v2, t2) = time(cache.get("a"))
    v2 mustBe "apple"
    t2 must be < 100L

    Thread.sleep(600L)

    val (v3, t3) = time(cache.get("a"))
    v3 mustBe "apricot"
    t3 must be < 100L

    cache.numberRefreshes must equal(2)
  }

  "forceRefresh" in {
    val cache = new TestCacheWithFallbackToStaleData[String]() {
      override def initialContents(): Seq[(String, String)] = Seq("a" -> "apple")
      refreshDelayMs = 500L
    }

    cache.set("a", "apricot")
    cache.forceRefresh("a")

    val (v2, t2) = time(cache.get("a"))
    v2 mustBe "apple"
    t2 must be < 100L

    Thread.sleep(600L)

    val (v3, t3) = time(cache.get("a"))
    v3 mustBe "apricot"
    t3 must be < 100L

    cache.numberRefreshes must equal(1)
  }

  "getIfPresent" in {
    val cache = new TestCacheWithFallbackToStaleData[String]() {
      refreshDelayMs = 500L
    }

    cache.set("a", "apple")

    val (v1, t1) = time(cache.getIfPresent("a"))
    v1 mustBe None
    t1 must be < 100L

    Thread.sleep(600L)

    val (v3, t3) = time(cache.getIfPresent("a"))
    v3 mustBe Some("apple")
    t3 must be < 100L

    cache.numberRefreshes must equal(1)
  }
}
