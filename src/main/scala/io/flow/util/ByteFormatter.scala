package io.flow.util

import java.math.RoundingMode
import java.text.DecimalFormat

object ByteFormatter {
  def byteCountSI(bytes: Long): String = {
    format(bytes, true)
  }

  def byteCountBinary(bytes: Long): String = {
    format(bytes, false)
  }

  private[util] val (df0, df1, df2) = {
    def withDownRound(df: DecimalFormat) = {
      df.setRoundingMode(RoundingMode.DOWN)
      df
    }
    (
      withDownRound(new DecimalFormat("0")),
      withDownRound(new DecimalFormat("0.#")),
      withDownRound(new DecimalFormat("0.##")),
    )
  }

  private def format(bytes: Long, si: Boolean) = {
    val absBytes = if (bytes == Long.MinValue) Long.MaxValue else Math.abs(bytes)
    val (baseValue, unitStrings) =
      if (si)
        (1000, Vector("k", "M", "G", "T", "P", "E", "Z", "Y"))
      else
        (1024, Vector("Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi", "Yi"))

    def getExponent(curBytes: Long, baseValue: Int, curExponent: Int = 0): Int = {
      if (curBytes < baseValue) {
        curExponent
      } else {
        val newExponent = 1 + curExponent
        getExponent(curBytes / baseValue, baseValue, newExponent)
      }
    }

    if (absBytes < baseValue) {
      s"$bytes B"
    } else {
      val signum = if (bytes < 0) "-" else ""
      val exponent = getExponent(absBytes, baseValue)
      val divisor = Math.pow(baseValue.toDouble, exponent.toDouble)
      val unitString = unitStrings(exponent - 1)

      val res = absBytes / divisor
      3 - res.toLong.toString.length match {
        case 2 => f"$signum${df2.format(res)} ${unitString}B"
        case 1 => f"$signum${df1.format(res)} ${unitString}B"
        case _ => f"$signum${df0.format(res)} ${unitString}B"
      }

    }
  }
}
