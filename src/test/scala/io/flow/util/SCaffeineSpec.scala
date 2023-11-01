package io.flow.util

import com.github.blemale.scaffeine.Scaffeine
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.nowarn
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class SCaffeineSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterEach {

  val callCount = new AtomicInteger(0)
  val tick = 150.millis
  val tickFuzz = 50L
  val tickMillis = tick.toMillis

  def fun(str: String): Int = {
    val ret = str.length + callCount.getAndIncrement()
    Thread.sleep(tickMillis)
    ret
  }

  @nowarn
  def failingFun(str: String): Int = {
    callCount.getAndIncrement()
    Thread.sleep(tickMillis)
    throw new RuntimeException("boom")
  }

  def reloadFailingFun(str: String): Int = {
    val count = callCount.getAndIncrement()
    val ret = str.length + count
    Thread.sleep(tickMillis)
    if (count > 0) {
      throw new RuntimeException("boom")
    } else {
      ret
    }
  }

  override def beforeEach() = {
    callCount.set(0)
  }

  "Scaffeine" when {
    "sync cache" should {
      "return the expected cache value" in {
        val cache = Scaffeine().build[String, Int](loader = { s: String => s.length })
        cache.get("one") mustBe 3
      }

      "getIfPresent does not load in the background" in {
        val cache = Scaffeine().build[String, Int](loader = { s: String => s.length })
        cache.getIfPresent("one") mustBe None
        Thread.sleep(tickFuzz)
        cache.getIfPresent("one") mustBe None
      }

      "getIfPresent with refresh loads in the background" in {
        val cache = Scaffeine().build[String, Int](loader = { s: String => s.length })
        cache.getIfPresent("one").orElse {
          cache.refresh("one")
          None
        } mustBe None
        Thread.sleep(tickFuzz)
        cache.getIfPresent("one") mustBe Some(3)
      }

      "refresh a cache value asynchronously" in {
        val cache = Scaffeine().refreshAfterWrite(tick).build[String, Int](loader = fun _)
        val (result1, ms1) = time[Int](cache.get("one"))
        result1 mustBe 3
        ms1 must be(tickMillis +- tickFuzz)

        // wait for key TTL to be expire
        Thread.sleep(tickMillis + tickFuzz)

        // lookup key will return old value and trigger refresh in the background
        val (result2, ms2) = time[Int](cache.get("one"))
        result2 mustBe 3
        ms2 must be < tickFuzz

        // wait for background refresh to have finished
        Thread.sleep(tickMillis + tickFuzz)

        // lookup will now return new value
        val (result3, ms3) = time[Int](cache.get("one"))
        result3 mustBe 4
        ms3 must be < tickFuzz

        // verify that lookup function was called twice
        callCount.get mustBe 2
      }

      "throw exception on first load" in {
        val cache = Scaffeine().build[String, Int](loader = failingFun _)
        a[RuntimeException] should be thrownBy cache.get("one")
        a[RuntimeException] should be thrownBy cache.get("one")
        callCount.get mustBe 2
      }

      "return previous value when an exception is thrown on reload" in {
        val cache = Scaffeine().refreshAfterWrite(tick).build[String, Int](loader = reloadFailingFun _)
        val (result1, ms1) = time[Int](cache.get("one"))
        result1 mustBe 3
        ms1 must be(tickMillis +- tickFuzz)

        // wait for key TTL to be expire
        Thread.sleep(tickMillis + tickFuzz)

        // lookup key will return old value and trigger refresh in the background
        val (result2, ms2) = time[Int](cache.get("one"))
        result2 mustBe 3
        ms2 must be < tickFuzz

        // wait for background refresh to have finished
        Thread.sleep(tickMillis + tickFuzz)

        // verify that lookup function was called twice
        callCount.get mustBe 2

        // lookup will still return cached initial value
        val (result3, ms3) = time[Int](cache.get("one"))
        result3 mustBe 3
        ms3 must be < tickFuzz
      }
    }

    "async cache" should {
      "return the expected cache value" in {
        val cache = Scaffeine().buildAsync[String, Int](loader = { s: String => s.length })
        val (result, ms) = time[Int](await(cache.get("one")))
        result mustBe 3
        ms must be < tickFuzz
      }

      "use allLoader" in {
        val cache = Scaffeine().buildAsync[String, Int](
          loader = (_: String) => 0,
          allLoader = Some((keys: Iterable[String]) => keys.map(key => key -> key.length).toMap)
        )
        val (result, _) = time[Map[String, Int]](await(cache.getAll(Seq("two", "three", "four"))))
        result.size mustBe 3
        result.get("two") mustBe Some(3)
        result.get("three") mustBe Some(5)
        result.get("four") mustBe Some(4)
      }

      "refresh a value asynchronously" in {
        val cache = Scaffeine().refreshAfterWrite(tick).buildAsync[String, Int](loader = fun _)
        val (f1, msf1) = time[Future[Int]](cache.get("one"))
        msf1 must be < tickFuzz
        val (result1, ms1) = time[Int](await(f1))
        result1 mustBe 3
        ms1 must be(tickMillis +- tickFuzz)

        // wait for key TTL to be expire
        Thread.sleep(tickMillis + tickFuzz)

        // lookup key will return old value and trigger refresh in the background
        val (result2, ms2) = time[Int](await(cache.get("one")))
        result2 mustBe 3
        ms2 must be < tickFuzz

        // wait for background refresh to have finished
        Thread.sleep(tickMillis + tickFuzz)

        // lookup will now return new value
        val (result3, ms3) = time[Int](await(cache.get("one")))
        result3 mustBe 4
        ms3 must be < tickFuzz

        // verify that lookup function was called twice
        callCount.get mustBe 2
      }

      "expire a value" in {
        @nowarn
        def create(s: String, i: Int) = {
          tick
        }

        @nowarn
        def read(s: String, i: Int, d: FiniteDuration) = {
          10.minutes
        }

        @nowarn
        def update(s: String, i: Int, d: FiniteDuration) = {
          10.minutes
        }

        val cache = Scaffeine().expireAfter(create _, update _, read _).buildAsync[String, Int](loader = fun _)
        val (result1, ms1) = time[Int](await(cache.get("one")))
        result1 mustBe 3
        ms1 must be(tickMillis +- tickFuzz)

        // wait for key TTL to be expire
        Thread.sleep(tickMillis + tickFuzz)

        // lookup key will trigger load of value
        val (result2, ms2) = time[Int](await(cache.get("one")))
        result2 mustBe 4
        ms2 must be(tickMillis +- tickFuzz)
      }

      "failed Future on failed first load" in {
        val cache = Scaffeine().buildAsync[String, Int](loader = failingFun _)
        val (f1, msf1) = time[Future[Int]](cache.get("one"))
        val (f2, msf2) = time[Future[Int]](cache.get("one"))
        msf1 must be < tickFuzz
        msf2 must be < tickFuzz

        a[RuntimeException] should be thrownBy await(f1)
        a[RuntimeException] should be thrownBy await(f2)

        Thread.sleep(tickFuzz)
        callCount.get mustBe 1

        val (f3, msf3) = time[Future[Int]](cache.get("one"))
        msf3 must be < tickFuzz
        a[RuntimeException] should be thrownBy await(f3)
        callCount.get mustBe 2
      }

      "return previous value when an exception is thrown on reload" in {
        val cache = Scaffeine().refreshAfterWrite(tick).buildAsync[String, Int](loader = reloadFailingFun _)
        val (result1, ms1) = time[Int](await(cache.get("one")))
        result1 mustBe 3
        ms1 must be(tickMillis +- tickFuzz)

        // wait for key TTL to be expire
        Thread.sleep(tickMillis + tickFuzz)

        // lookup key will return old value and trigger refresh in the background
        val (result2, ms2) = time[Int](await(cache.get("one")))
        result2 mustBe 3
        ms2 must be < tickFuzz

        // wait for background refresh to have finished
        Thread.sleep(tickMillis + tickFuzz)

        // verify that lookup function was called twice
        callCount.get mustBe 2

        // lookup will still return cached initial value
        val (result3, ms3) = time[Int](await(cache.get("one")))
        result3 mustBe 3
        ms3 must be < tickFuzz
      }
    }

    "sync view of async cache" should {
      "loads in async cache propagate to sync cache" in {
        val asyncCache = Scaffeine().refreshAfterWrite(tick).buildAsync[String, Int](loader = fun _)
        val syncCache = asyncCache.synchronous()
        val (result1, ms1) = time[Int](await(asyncCache.get("one")))
        result1 mustBe 3
        ms1 must be(tickMillis +- tickFuzz)

        val (result2, ms2) = time[Int](syncCache.get("one"))
        result2 mustBe 3
        ms2 must be < tickFuzz

        // wait for key TTL to be expire
        Thread.sleep(tickMillis + tickFuzz)

        // lookup key will return old value and trigger refresh in the background
        val (result3, ms3) = time[Int](await(asyncCache.get("one")))
        result3 mustBe 3
        ms3 must be < tickFuzz

        // wait for background refresh to have finished
        Thread.sleep(tickMillis + tickFuzz)

        val (result4, ms4) = time[Int](syncCache.get("one"))
        result4 mustBe 4
        ms4 must be < tickFuzz

        callCount.get mustBe 2
      }

      "loads in sync cache propagate to async cache" in {
        val asyncCache = Scaffeine().refreshAfterWrite(tick).buildAsync[String, Int](loader = fun _)
        val syncCache = asyncCache.synchronous()
        val (result1, ms1) = time[Int](syncCache.get("one"))
        result1 mustBe 3
        ms1 must be(tickMillis +- tickFuzz)

        val (result2, ms2) = time[Int](await(asyncCache.get("one")))
        result2 mustBe 3
        ms2 must be < tickFuzz

        // wait for key TTL to be expire
        Thread.sleep(tickMillis + tickFuzz)

        // lookup key will return old value and trigger refresh in the background
        val (result3, ms3) = time[Int](syncCache.get("one"))
        result3 mustBe 3
        ms3 must be < tickFuzz

        // wait for background refresh to have finished
        Thread.sleep(tickMillis + tickFuzz)

        val (result4, ms4) = time[Int](await(asyncCache.get("one")))
        result4 mustBe 4
        ms4 must be < tickFuzz

        callCount.get mustBe 2
      }

      "puts in async cache propagate to sync cache" in {
        val asyncCache = Scaffeine().refreshAfterWrite(tick).buildAsync[String, Int](loader = (_: String) => 0)
        val syncCache = asyncCache.synchronous()
        asyncCache.put("one", Future(fun("one")))
        val (result, ms) = time[Int](syncCache.get("one"))
        result mustBe 3
        ms must be(tickMillis +- tickFuzz)
      }

      "puts in sync cache propagate to async cache" in {
        val asyncCache = Scaffeine().refreshAfterWrite(tick).buildAsync[String, Int](loader = (_: String) => 0)
        val syncCache = asyncCache.synchronous()
        syncCache.put("one", 3)
        val (result1, ms1) = time[Int](await(asyncCache.get("one")))
        result1 mustBe 3
        ms1 must be < tickFuzz
      }
    }
  }

  def await[T](future: Future[T]): T = Await.result(future, 10.seconds)

  private def time[T](f: => T): (T, Long) = {
    val start = System.currentTimeMillis()
    val ret: T = f
    val elapsed = System.currentTimeMillis() - start
    (ret, elapsed)
  }
}
