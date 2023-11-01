package io.flow.util

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AllocatorSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  private val elementGen: Gen[BigDecimal] = Gen.choose(1, 1000000).map(BigDecimal(_) / 100)
  private val elementsGen: Gen[List[BigDecimal]] = Gen.nonEmptyListOf(elementGen)
  private val amountGen: Gen[BigDecimal] = Gen.choose(-10000000, 10000000).suchThat(_ != 0).map(BigDecimal(_) / 100)
  private val amountIntGen: Gen[Int] = Gen.choose(-10000000, 10000000).suchThat(_ != 0)
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

    "negative amount, positive proportions" in {
      val elements: Seq[BigDecimal] = Seq(10, 10, 10)
      val res = Allocator.proportionallyAllocate(-10, elements, scale = Some(2))
      res shouldBe Seq(-3.34, -3.33, -3.33)
    }

    "positive amount, negative proportions" in {
      val elements: Seq[BigDecimal] = Seq(-10, -10, -10)
      val res = Allocator.proportionallyAllocate(10, elements, scale = Some(2))
      res shouldBe Seq(3.34, 3.33, 3.33)
    }

    "negative amount, negative proportions" in {
      val elements: Seq[BigDecimal] = Seq(-10, -10, 10)
      val res = Allocator.proportionallyAllocate(-10, elements, scale = Some(2))
      res shouldBe Seq(-3.34, -3.33, -3.33)
    }

    "negative amount, negative and positive proportions" in {
      val elements: Seq[BigDecimal] = Seq(-10, 10, 10)
      val res = Allocator.proportionallyAllocate(-10, elements, scale = Some(2))
      res shouldBe Seq(-3.34, -3.33, -3.33)
    }

    "positive amount, negative and positive proportions" in {
      val elements: Seq[BigDecimal] = Seq(-10, 10, 10)
      val res = Allocator.proportionallyAllocate(10, elements, scale = Some(2))
      res shouldBe Seq(3.34, 3.33, 3.33)
    }

    "0 amount invariant" in {
      forAll(elementsGen, scaleGen, minSuccessful(10000)) { case (elements, scale) =>
        val res = Allocator.proportionallyAllocate(0, elements, scale = Some(scale))
        res.size shouldBe elements.size
        res shouldBe Seq.fill(elements.size)(0)
      }
    }

    "size, signum and sum invariant" in {
      forAll(amountGen, elementsGen, scaleGen, minSuccessful(10000)) { case (amount, elements, scale) =>
        val res = Allocator.proportionallyAllocate(amount, elements, scale = Some(scale))
        res.size shouldBe elements.size
        res.map(_.signum).distinct.filter(_ != 0) shouldBe Seq(amount.signum)
        res.sum shouldBe amount
      }
    }

    "size, sum invariant for ints" in {
      forAll(amountIntGen, elementsGen, minSuccessful(10000)) { case (amount, elements) =>
        val res = Allocator.proportionallyAllocateInts(amount, elements)
        res.size shouldBe elements.size
        res.sum shouldBe amount
      }
    }

    "proportionallyAllocateInts" in {
      def test(proportions: Seq[BigDecimal]) = Allocator.proportionallyAllocateInts(100, proportions)

      test(Seq(1, 1)) shouldBe Seq(50, 50)
      test(Seq(1, 1, 1)) shouldBe Seq(34, 33, 33)
      test(Seq(1, 2)) shouldBe Seq(33, 67)
    }
  }
}
