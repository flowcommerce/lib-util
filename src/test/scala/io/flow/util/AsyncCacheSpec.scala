package io.flow.util

import org.scalatest.concurrent.Eventually.{eventually, timeout}
import org.scalatest.matchers.must.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class AsyncCacheSpec extends AnyWordSpecLike with Matchers {
  implicit val executionContext = ExecutionContext.global

  private[this] class TestAsyncCache[T]() extends AsyncCache[String, T] {

    private[this] val data = new ConcurrentHashMap[String, Future[Option[T]]]()
    private val refreshes = new AtomicInteger()
    private val firstTimers = new AtomicInteger()

    def numberRefreshes: Int = refreshes.intValue()
    def numberFirstTimeRefreshes: Int = firstTimers.intValue()

    override def refresh(key: String, firstTime: Boolean):  Future[Option[T]] = {
      refreshes.incrementAndGet()
      if (firstTime) firstTimers.incrementAndGet()
      Option(data.get(key)).getOrElse(Future.failed(new RuntimeException("Missing entry")))
    }

    override def durationForNone: FiniteDuration = FiniteDuration(1, SECONDS)

    def set(key: String, value: Future[Option[T]]): Unit = {
      data.put(key, value)
      ()
    }
  }

  private[this] def eventuallyInNSeconds[T](n: Int)(f: => T): T = {
    eventually(timeout(Span(n.toLong, Seconds))) {
      f
    }
  }

  private def slowFuture[V](f: => Option[V])(implicit delay: FiniteDuration = FiniteDuration(1, SECONDS)): Future[Option[V]] = Future {
    Thread.sleep(delay.toMillis)
    f
  }

  "cached values are served" in {
    val cache = new TestAsyncCache[String]() {
      override val duration: FiniteDuration = FiniteDuration(1, SECONDS)
    }
    cache.set("a", Future.successful(Some("apple")))

    // Test cache hit
    cache.get("a") must equal(Some("apple"))
    cache.get("a") must equal(Some("apple"))
    cache.numberRefreshes must equal(1)

    // Test cache miss and auto refresh after 1 second
    eventuallyInNSeconds(5) {
      cache.get("a") must equal(Some("apple"))
      cache.numberRefreshes must equal(2)
    }
    cache.numberFirstTimeRefreshes must equal(1)
  }

  "supports multiple keys" in {
    val cache = new TestAsyncCache[String]()
    cache.set("a", Future.successful(Some("apple")))
    cache.set("p", Future.successful(Some("pear")))
    cache.set("x", Future.successful(None))
    cache.set("y", Future.failed(new RuntimeException("boom!")))

    cache.get("a") must equal(Some("apple"))
    cache.get("p") must equal(Some("pear"))
    cache.get("x") must equal(None)
    cache.get("y") must equal(None)
    cache.get("z") must equal(None)
    cache.numberRefreshes must equal(5)
    cache.numberFirstTimeRefreshes must equal(5)
  }

  "supports bootstrap dataset" in {
    val cache = new TestAsyncCache[String]() {
      override def initialContents() = Seq(("a" -> "apple"), ("p" -> "pear"))
    }

    cache.get("a") must equal(Some("apple"))
    cache.get("p") must equal(Some("pear"))
    cache.numberRefreshes must equal(0)
    cache.numberFirstTimeRefreshes must equal(0)
  }

  "failed refresh handled gracefully" in {
    val cache = new TestAsyncCache[String]() {
      override val duration: FiniteDuration = FiniteDuration(1, SECONDS)
    }
    cache.set("a", Future.successful(Some("apple")))
    cache.get("a") must equal(Some("apple"))

    cache.set("a", Future.failed(new RuntimeException("boom!")))

    Thread.sleep(2000)

    // failing refresh should return old value
    cache.get("a") must equal(Some("apple"))
    cache.numberRefreshes must equal(2)
  }

  "flushed key serves stale data if refresh fails" in {
    val cache = new TestAsyncCache[String]()
    cache.set("a", Future.successful(Some("apple")))
    cache.get("a") must equal(Some("apple"))
    cache.flush("a")
    cache.set("a", Future.failed(new RuntimeException("boom!")))

    // failing refresh should return old value
    cache.get("a") must equal(Some("apple"))
    cache.numberRefreshes must equal(2)
    cache.numberFirstTimeRefreshes must equal(1)
  }

  "flushed key is immediately refreshed" in {
    val cache = new TestAsyncCache[String]()
    cache.set("a", Future.successful(Some("apple")))
    cache.get("a") must equal(Some("apple"))
    cache.flush("a")
    cache.set("a", Future.successful(Some("foo")))

    // successful refresh should return new value
    cache.get("a") must equal(Some("foo"))
    cache.numberRefreshes must equal(2)
    cache.numberFirstTimeRefreshes must equal(1)
  }

  "Caching none is cached for short period of time" in {
    val cache = new TestAsyncCache[String]()
    cache.set("a", Future.successful(None))
    cache.get("a") must equal(None)
    cache.set("a", Future.successful(Some("apple")))

    eventuallyInNSeconds(2) {
      cache.get("a") must equal(Some("apple"))
    }
    cache.numberRefreshes must equal(2)
    cache.numberFirstTimeRefreshes must equal(1)
  }

  "Concurrent get should refresh only once" in {
    val cache = new TestAsyncCache[String]() {
      override val duration: FiniteDuration = FiniteDuration(2, SECONDS)
      override val durationForNone: FiniteDuration = FiniteDuration(2, SECONDS)
    }
    cache.set("a", slowFuture(Some("apple")))

    val get1 = Future { cache.get("a") }
    val get2 = Future { cache.get("a") }
    Await.result(get1, 100.millis) must equal(None)
    Await.result(get2, 100.millis) must equal(None)
    cache.numberRefreshes must be(1)

    eventuallyInNSeconds(2) {
      val get3 = Future { cache.get("a") }
      val get4 = Future { cache.get("a") }
      Await.result(get3, 100.millis) must equal(Some("apple"))
      Await.result(get4, 100.millis) must equal(Some("apple"))
    }
    cache.numberRefreshes must be(1)

    cache.flush("a")
    cache.set("a", slowFuture(Some("pear")))

    val get5 = Future { cache.get("a") }
    val get6 = Future { cache.get("a") }
    Await.result(get5, 100.millis) must equal(Some("apple"))
    Await.result(get6, 100.millis) must equal(Some("apple"))
    cache.numberRefreshes must be(2)

    eventuallyInNSeconds(2) {
      val get7 = Future { cache.get("a") }
      val get8 = Future { cache.get("a") }
      Await.result(get7, 100.millis) must equal(Some("pear"))
      Await.result(get8, 100.millis) must equal(Some("pear"))
    }
    cache.numberRefreshes must be(2)
    cache.numberFirstTimeRefreshes must equal(1)
  }
}
