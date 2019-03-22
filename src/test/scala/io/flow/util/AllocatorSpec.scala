package io.flow.util

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}


class AllocatorSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  private val elementGen: Gen[BigDecimal] = Gen.choose(1, 1000000).map(BigDecimal(_)/ 100)
  private val elementsGen: Gen[List[BigDecimal]] = Gen.nonEmptyListOf(elementGen)
  private val amountGen: Gen[BigDecimal] = Gen.choose(1, 10000000).map(BigDecimal(_) / 100)
  private val scaleGen: Gen[Int] = Gen.choose(-3, 3)

  "Allocator" should {

    "one element" in {
      val elements: Seq[BigDecimal] = Seq(1)
      val res = Allocator.proportionallyAllocate(10, elements)
      res shouldBe Seq(10)
    }

    "multiple elements exact - 1" in {
      val elements: Seq[BigDecimal] = Seq(50, 6)
      val res = Allocator.proportionallyAllocate(10, elements)
      res shouldBe Seq(BigDecimal(50) * 10 / 56, BigDecimal(6) * 10 / 56)
    }

    "multiple elements exact - 2" in {
      val elements: Seq[BigDecimal] = Seq(10, 10, 20)
      val res = Allocator.proportionallyAllocate(10, elements)
      res shouldBe Seq(2.5, 2.5, 5)
    }

    "multiple elements rounded - 0" in {
      val elements: Seq[BigDecimal] = Seq(10, 10, 20)
      val res = Allocator.proportionallyAllocate(10, elements, scale = Some(0))
      res shouldBe Seq(3, 2, 5)
    }

    "multiple elements rounded - 1" in {
      val elements: Seq[BigDecimal] = Seq(50, 6)
      val res = Allocator.proportionallyAllocate(10, elements, scale = Some(2))

      // 50 * 10 / 56 = 8.928571429
      // 6 * 10 / 56 = 1.071428571
      res shouldBe Seq(8.93, 1.07)
    }

    "multiple elements rounded - 2" in {
      val elements: Seq[BigDecimal] = Seq(10, 10, 10)
      val res = Allocator.proportionallyAllocate(10, elements, scale = Some(2))
      res shouldBe Seq(3.34, 3.33, 3.33)
    }

    "multiple elements rounded - 3" in {
      val elements: Seq[BigDecimal] = Seq(10, 10, 10)
      val res = Allocator.proportionallyAllocate(20, elements, scale = Some(2))
      res shouldBe Seq(6.67, 6.67, 6.66)
    }

    "edge case - 1" in {
      val elements: Seq[BigDecimal] = Seq(50, 6)
      val res = Allocator.proportionallyAllocate(10, elements, scale = Some(-2))

      // 50 * 10 / 56 = 8.928571429
      // 6 * 10 / 56 = 1.071428571
      res shouldBe Seq(10, 0)
    }

    "sum invariant" in {
      forAll(amountGen, elementsGen, scaleGen, minSuccessful(10000)) { case (amount, elements, scale) =>
        Allocator.proportionallyAllocate(amount, elements, scale = Some(scale)).sum shouldBe amount
      }
    }
  }
}


