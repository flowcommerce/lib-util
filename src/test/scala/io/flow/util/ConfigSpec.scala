package io.flow.util

import org.scalatest.{MustMatchers, TryValues, WordSpec}

import scala.concurrent.duration._

class ConfigSpec extends WordSpec with MustMatchers with TryValues {
  def mockConfig(memory: (String, String)*): Config = new Config {
    val memoryMap: Map[String, String] = memory.toMap

    override def get(name: String): Option[String] = memoryMap.get(name)

    override def optionalMap(name: String): Option[Map[String, Seq[String]]] = None //unsupported in this test

    override def optionalList(name: String): Option[Seq[String]] = None //unsupported in this test
  }

  val config: Config = mockConfig(
    "negative" -> "-1 second",
    "invalid-pattern" -> "about 5 minutes",
    "invalid-pattern-2" -> "n minutes",
    "invalid-unit" -> "5 years",
    "zero" -> "0 minutes",
    "valid-value-1" -> "10 minutes",
    "valid-value-2" -> "1 second"
  )

  def getDuration(name: String): Option[FiniteDuration] = {
    config.optionalFiniteDuration(name)
  }

  def exceptionMessage(name: String): String = {
    intercept[Exception](getDuration(name)).getMessage
  }

  "optionalDuration" when {
    "the field isn't there" should {
      "return None" in {
        getDuration("gone") mustBe empty
      }
    }

    "the amount is negative" should {
      "fail" in {
        exceptionMessage("negative") mustBe
          "FlowError Configuration variable[negative] has invalid value[-1 second]. Underlying cause: Invalid pattern of FiniteDuration"
      }
    }

    "the field doesn't match the pattern" should {
      "fail" in {
        exceptionMessage("invalid-pattern") mustBe
          "FlowError Configuration variable[invalid-pattern] has invalid value[about 5 minutes]. Underlying cause: Invalid pattern of FiniteDuration"

        exceptionMessage("invalid-pattern-2") mustBe
          "FlowError Configuration variable[invalid-pattern-2] has invalid value[n minutes]. Underlying cause: Invalid pattern of FiniteDuration"
      }
    }

    "the field has an invalid unit" should {
      "fail" in {
        exceptionMessage("invalid-unit") mustBe
          "FlowError Configuration variable[invalid-unit] has invalid value[5 years]. Underlying cause: years not supported"
      }
    }

    "the amount is zero" should {
      "be ok" in {
        getDuration("zero").get mustBe 0.seconds
      }
    }

    "the amount is 10 minutes" should {
      "be ok" in {
        getDuration("valid-value-1").get mustBe 10.minutes
      }
    }

    "the amount is 1 second" should {
      "be ok" in {
        getDuration("valid-value-2").get mustBe 1.second
      }
    }
  }
}

class EnvironmentConfigLikeSpec extends WordSpec with MustMatchers {
  val mockConfig: Config = new EnvironmentConfigLike {
    override protected def sourceName: String = "mock"

    override protected def source(): Map[String, String] = Map(
      "HELLO_WORLD_FOO" -> "bar, baz",
      "HELLO_WORLD_BAR_BAZ" -> "foo",
      "HELLO_ME" -> "w00t"
    )
  }

  "optionalMap" should {
    "transform keys and values correctly" in {
      val result = mockConfig.optionalMap("HELLO_WORLD").get

      result mustBe Map(
        "foo" -> List("bar", "baz"),
        "bar.baz" -> List("foo")
      )
    }
  }
}
