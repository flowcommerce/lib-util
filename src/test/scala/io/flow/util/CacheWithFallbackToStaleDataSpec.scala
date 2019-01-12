package io.flow.util

import org.scalatest.{MustMatchers, WordSpecLike}
import org.scalatest.concurrent.Eventually.{eventually, timeout}
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.duration._

class CacheWithFallbackToStaleDataSpec extends WordSpecLike with MustMatchers {

  private[this] case class TestCacheWithFallbackToStaleData[T]() extends CacheWithFallbackToStaleData[String, T] {

    private[this] val data = scala.collection.mutable.Map[String, T]()
    private[this] val nextValues = scala.collection.mutable.Map[String, T]()
    var numberRefreshes: Int = 0
    var refreshShouldFail = false

    override def refresh(key: String): T = {
      if (!refreshShouldFail) {
        numberRefreshes += 1
        data += key -> nextValues.getOrElse(
          key,
          data.getOrElse(key, sys.error(s"Missing test data for key[$key]"))
        )
      }

      data.getOrElse(key, sys.error(s"Missing test data for key[$key]"))
    }

    def setNextValue(key: String, value: T): Unit = {
      nextValues += (key -> value)
    }

    def set(key: String, value: T): Unit = {
      data += (key -> value)
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

}
