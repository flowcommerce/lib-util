package io.flow.util

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.annotation.nowarn

@nowarn
class IdGeneratorSpec extends AnyWordSpecLike with Matchers {

  private[this] val MinimumRandomLength = 16

  "prefix must be 3 characters" in {
    intercept[AssertionError] {
      IdGenerator("fo")
    }.getMessage must be("assertion failed: prefix[fo] must be 3 characters long")
  }

  "prefix must be lower case" in {
    intercept[AssertionError] {
      IdGenerator("FOO")
    }.getMessage must be("assertion failed: prefix[FOO] must be in lower case")
  }

  "prefix must be trimmed" in {
    intercept[AssertionError] {
      IdGenerator("  foo  ")
    }.getMessage must be("assertion failed: prefix[  foo  ] must be trimmed")
  }

  "prefix is not on black list" in {
    intercept[AssertionError] {
      IdGenerator("ass")
    }.getMessage must be("assertion failed: prefix[ass] is on the black list and cannot be used")
  }

  "randomId must start with prefix" in {
    val generator = IdGenerator("tst")
    val id = generator.randomId()
    id.startsWith("tst-") must be(true)
  }

  "uuid from bytes" in {
    val id1 = "exp-b476512e183944feb77126843ecb0271"
    val id2 = "itm-a9308bbbf5c3431bb2fd7c72c92d9d62"

    val generator = IdGenerator("tst")
    val id = generator.fromBytes((id1 + id2).getBytes)

    id mustBe "tst-54622b3b58be361c93522bb9be6468a3"
  }

  "randomId" in {
    val generator = IdGenerator("tst")
    val num = 10000
    val ids = 1.to(num).map { _ => generator.randomId() }
    ids.size must be(num)
    ids.distinct.size must be(ids.size)
    ids.foreach { _.length must be >= (MinimumRandomLength) }
  }

  "format" in {
    val generator = IdGenerator("tst")
    val id = generator.randomId()
    id.split("-").toList match {
      case prefix :: uuid :: Nil => {
        prefix must be("tst")
        uuid.length must be(32)
      }
      case _ => {
        sys.error("Expected one dash")
      }
    }
  }

  "validate" in {
    val generator = IdGenerator("tst")
    generator.validate(generator.randomId()) mustBe true
    generator.validate(generator.randomId().replace("-", "_")) mustBe false
    generator.validate(s"x${generator.randomId()}") mustBe false
    generator.validate(s"${generator.randomId()}g") mustBe false
    generator.validate(generator.randomId().tail) mustBe false
    generator.validate(generator.randomId().init) mustBe false
    generator.validate(generator.randomId().init) mustBe false
    generator.validate(s"x${generator.randomId().tail}") mustBe false
    generator.validate(s"${generator.randomId().init}g") mustBe false
    generator.validate("") mustBe false
    generator.validate(generator.randomId() + generator.randomId()) mustBe false
  }
}
