package io.flow.util

import scala.concurrent.duration.FiniteDuration

object DurationFormatter {
  import ByteFormatter.{df0, df1, df2}
  private val units = Seq((1000L, "ns"), (1000L, "us"), (1000L, "ms"), (60L, "sec"), (60L, "min"), (24L, "h"))

  def format(duration: FiniteDuration): String = {
    val nanos = duration.toNanos.toDouble
    val (t, u) = units.foldLeft((nanos, None: Option[String])) { case ((time, result), (base, unit)) =>
      result.fold {
        if (time < base) {
          (time, Some(unit))
        } else {
          (time / base, None)
        }
      }(_ => (time, result))
    }
    val unit = u.getOrElse("days")
    3 - t.toLong.toString.length match {
      case 2 => f"${df2.format(t)} $unit"
      case 1 => f"${df1.format(t)} $unit"
      case _ => f"${df0.format(t)} $unit"
    }
  }
}
