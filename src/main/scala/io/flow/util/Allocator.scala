package io.flow.util

import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode

object Allocator {

  /**
    * Proportionally allocates the given amount to the elements.
    * Returns a Seq whose size is the same as the elements.
    *
    * The sum of the returned elements is always equal to given amount.
    *
    * If the amount is positive, the returned proportions will be positive. If the amount is negative, the returned
    * proportions will be negative.
    *
    * If the elements are empty, an empty Seq is returned.
    *
    * The elements can be of either positive, negative, or both. Only their distance to 0 (the absolute value) will be
    * considered to compute the proportions
    *
    * For instance:
    * (6, Seq(10, 20)) => Seq(2, 4)
    * (10, Seq(1, 1, 1), scale = 2) => Seq(3.34, 3.33, 3.33)
    * (10, Seq(1, 1, 1)) => Seq(3.33333333333, 3.33333333333, 3.33333333333)
    * (20, Seq(1, 1, 1), scale = 2) => Seq(6.67, 6.67, 6.66)
    * (10, Seq(50, 6), scale = -2) => Seq(10, 0)   // 50 * 10 / 56 = 8.928571429 and 6 * 10 / 56 = 1.071428571
    * (-6, Seq(10, 20)) => Seq(-2, -4)
    * (6, Seq(-10, -20)) => Seq(2, 4)
    * (-6, Seq(10, -20)) => Seq(-2, -4)
    * (6, Seq(10, -20)) => Seq(2, 4)
    *
    * @param amount the amount to proportionally allocate
    * @param proportions the proportions use to allocate the given amount
    * @param scale the scale used to round the allocated amounts. None means no rounding.
    */
  def proportionallyAllocate(
    amount: BigDecimal,
    proportions: Seq[BigDecimal],
    scale: Option[Int] = None
  ): Seq[BigDecimal] = scale match {
    case None => proportionallyAllocateExact(amount, proportions)
    case Some(s) => proportionallyAllocateRound(amount, proportions, s)
  }

  /**
    * @see [[proportionallyAllocate]]
    */
  def proportionallyAllocateExact(amount: BigDecimal, proportions: Seq[BigDecimal]): Seq[BigDecimal] = {
    val positiveProportions = proportions.map(_.abs)
    val total = positiveProportions.sum
    if (positiveProportions.isEmpty)
      Seq.empty
    else if (total == 0) {
      val size = positiveProportions.size
      positiveProportions.map(_ => amount / size)
    } else
      positiveProportions.map(v => (v * amount) / total)
  }

  /**
    * @see [[proportionallyAllocate]]
    */
  def proportionallyAllocateRound(amount: BigDecimal, proportions: Seq[BigDecimal], scale: Int): Seq[BigDecimal] = {
    val positiveProportions = proportions.map(_.abs)
    val positiveAmount = amount.abs
    val total = positiveProportions.sum

    if (positiveProportions.isEmpty)
      Seq.empty
    else if (total == 0) {
      val size = positiveProportions.size
      positiveProportions.map(_ => amount / size)
    } else {

      val distances = positiveProportions.zipWithIndex.map { case (proportion, index) =>
        val exact = (proportion * positiveAmount) / total
        val floored = exact.setScale(scale, RoundingMode.DOWN)
        val distance = (floored - exact).abs
        (floored, distance, index)
      }

      val totalRounded = distances.map { case (floored, _, _) => floored }.sum
      val delta = positiveAmount - totalRounded

      // give priority to the elements with the highest distance from the floored value
      val distancesSorted = distances.sortBy { case (_, distance, _) => -distance }

      val step = BigDecimal(10).pow(-scale)
      val zero = (delta, mutable.ListBuffer[(Int, BigDecimal)]())
      val (_, allocatedDeltas) = distancesSorted.foldLeft(zero) { case ((remaining, acc), (_, _, index)) =>
        if (remaining <= 0)
          (remaining, acc)
        else {
          val toAllocate = step.min(remaining.abs)
          acc += (index -> toAllocate)
          val newRemaining = remaining - toAllocate
          (newRemaining, acc)
        }
      }

      val grouped = allocatedDeltas.groupBy { case (i, _) => i }.mapValues(_.head).mapValues { case (_, d) => d }

      distances
        .map { case (r, _, i) => r + grouped.getOrElse(i, 0) }
        .map(_ * amount.signum)
    }
  }

}
