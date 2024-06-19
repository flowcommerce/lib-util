package io.flow.util

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.concurrent.duration._

class DurationFormatterSpec extends AnyWordSpec with Matchers {
  "nanosecond values" in {
    DurationFormatter.format(0.nanos) mustBe "0 ns"
    DurationFormatter.format(1.nanos) mustBe "1 ns"
    DurationFormatter.format(9.nanos) mustBe "9 ns"
    DurationFormatter.format(10.nanos) mustBe "10 ns"
    DurationFormatter.format(99.nanos) mustBe "99 ns"
    DurationFormatter.format(100.nanos) mustBe "100 ns"
    DurationFormatter.format(999.nanos) mustBe "999 ns"
  }

  "microsecond values" in {
    DurationFormatter.format(1000.nanos) mustBe "1 us"
    DurationFormatter.format(1009.nanos) mustBe "1 us"
    DurationFormatter.format(1010.nanos) mustBe "1.01 us"
    DurationFormatter.format(1099.nanos) mustBe "1.09 us"
    DurationFormatter.format(1100.nanos) mustBe "1.1 us"
    DurationFormatter.format(9999.nanos) mustBe "9.99 us"
    DurationFormatter.format(10000.nanos) mustBe "10 us"
    DurationFormatter.format(10099.nanos) mustBe "10 us"
    DurationFormatter.format(10100.nanos) mustBe "10.1 us"
    DurationFormatter.format(99999.nanos) mustBe "99.9 us"
    DurationFormatter.format(100000.nanos) mustBe "100 us"
    DurationFormatter.format(999999.nanos) mustBe "999 us"
  }

  "millisecond values" in {
    DurationFormatter.format(1.milli) mustBe "1 ms"
    DurationFormatter.format(1000000.nanos) mustBe "1 ms"
    DurationFormatter.format(999.millis) mustBe "999 ms"
    DurationFormatter.format(999999999.nanos) mustBe "999 ms"
  }

  "second values" in {
    DurationFormatter.format(1.second) mustBe "1 sec"
    DurationFormatter.format(1000.millis) mustBe "1 sec"
    DurationFormatter.format(1000000000.nanos) mustBe "1 sec"
    DurationFormatter.format(59.seconds) mustBe "59 sec"
    DurationFormatter.format(59999.millis) mustBe "59.9 sec"
    DurationFormatter.format(59999999999L.nanos) mustBe "59.9 sec"
  }

  "minute values" in {
    DurationFormatter.format(1.minute) mustBe "1 min"
    DurationFormatter.format(60.seconds) mustBe "1 min"
    DurationFormatter.format(60000000000L.nanos) mustBe "1 min"
    DurationFormatter.format(59.minutes) mustBe "59 min"
    DurationFormatter.format(3599.seconds) mustBe "59.9 min"
    DurationFormatter.format(3599999999999L.nanos) mustBe "59.9 min"
  }

  "hour values" in {
    DurationFormatter.format(1.hour) mustBe "1 h"
    DurationFormatter.format(60.minutes) mustBe "1 h"
    DurationFormatter.format(3600000000000L.nanos) mustBe "1 h"
    DurationFormatter.format(23.hours) mustBe "23 h"
    DurationFormatter.format(1439.minutes) mustBe "23.9 h"
    DurationFormatter.format(86399999999999L.nanos) mustBe "23.9 h"
  }

  "day values" in {
    DurationFormatter.format(1.day) mustBe "1 days"
    DurationFormatter.format(24.hours) mustBe "1 days"
    DurationFormatter.format(86400000000000L.nanos) mustBe "1 days"
    DurationFormatter.format(100000.days) mustBe "100000 days"
  }
}
