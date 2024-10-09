package io.flow.util

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ByteFormatterSpec extends AnyWordSpec with Matchers {
  "SI byte count" should {
    "byte values" in {
      ByteFormatter.byteCountSI(0L) mustBe "0 B"
      ByteFormatter.byteCountSI(1L) mustBe "1 B"
      ByteFormatter.byteCountSI(10L) mustBe "10 B"
      ByteFormatter.byteCountSI(100L) mustBe "100 B"
      ByteFormatter.byteCountSI(999L) mustBe "999 B"
    }

    "KB values" in {
      ByteFormatter.byteCountSI(1000L) mustBe "1 kB"
      ByteFormatter.byteCountSI(1009L) mustBe "1 kB"
      ByteFormatter.byteCountSI(1010L) mustBe "1.01 kB"
      ByteFormatter.byteCountSI(9999L) mustBe "9.99 kB"
      ByteFormatter.byteCountSI(10000L) mustBe "10 kB"
      ByteFormatter.byteCountSI(10099L) mustBe "10 kB"
      ByteFormatter.byteCountSI(10100L) mustBe "10.1 kB"
      ByteFormatter.byteCountSI(99999L) mustBe "99.9 kB"
      ByteFormatter.byteCountSI(100000L) mustBe "100 kB"
      ByteFormatter.byteCountSI(100999L) mustBe "100 kB"
      ByteFormatter.byteCountSI(101000L) mustBe "101 kB"
      ByteFormatter.byteCountSI(999999L) mustBe "999 kB"
    }

    "MB values" in {
      ByteFormatter.byteCountSI(1000000L) mustBe "1 MB"
      ByteFormatter.byteCountSI(999999999L) mustBe "999 MB"
    }

    "GB values" in {
      ByteFormatter.byteCountSI(1000000000L) mustBe "1 GB"
      ByteFormatter.byteCountSI(999999999999L) mustBe "999 GB"
    }

    "TB values" in {
      ByteFormatter.byteCountSI(1000000000000L) mustBe "1 TB"
      ByteFormatter.byteCountSI(999999999999999L) mustBe "999 TB"
    }

    "Extreme values" in {
      ByteFormatter.byteCountSI(Long.MinValue) mustBe "-9.22 EB"
      ByteFormatter.byteCountSI(Long.MaxValue) mustBe "9.22 EB"
    }
  }

  "Binary byte count" should {
    "byte values" in {
      ByteFormatter.byteCountBinary(0L) mustBe "0 B"
      ByteFormatter.byteCountBinary(1L) mustBe "1 B"
      ByteFormatter.byteCountBinary(10L) mustBe "10 B"
      ByteFormatter.byteCountBinary(100L) mustBe "100 B"
      ByteFormatter.byteCountBinary(999L) mustBe "999 B"
      ByteFormatter.byteCountBinary(1023L) mustBe "1023 B"
    }

    "KiB values" in {
      ByteFormatter.byteCountBinary(1024L) mustBe "1 KiB"
      ByteFormatter.byteCountBinary(1034L) mustBe "1 KiB"
      ByteFormatter.byteCountBinary(1035L) mustBe "1.01 KiB"
      ByteFormatter.byteCountBinary(1024L * 10 - 1) mustBe "9.99 KiB"
      ByteFormatter.byteCountBinary(1024L * 10) mustBe "10 KiB"
      ByteFormatter.byteCountBinary(1024L * 11 - 1) mustBe "10.9 KiB"
      ByteFormatter.byteCountBinary(1024L * 11) mustBe "11 KiB"
      ByteFormatter.byteCountBinary(1024L * 100 - 1) mustBe "99.9 KiB"
      ByteFormatter.byteCountBinary(1024L * 100) mustBe "100 KiB"
      ByteFormatter.byteCountBinary(1024L * 128 - 1) mustBe "127 KiB"
      ByteFormatter.byteCountBinary(1024L * 128) mustBe "128 KiB"
      ByteFormatter.byteCountBinary(1024L * 1024 - 1) mustBe "1023 KiB"
    }

    "MiB values" in {
      ByteFormatter.byteCountBinary(1024L * 1024) mustBe "1 MiB"
      ByteFormatter.byteCountBinary(1024L * 1024 * 1024 - 1) mustBe "1023 MiB"
    }

    "GiB values" in {
      ByteFormatter.byteCountBinary(1024L * 1024 * 1024) mustBe "1 GiB"
      ByteFormatter.byteCountBinary(1024L * 1024 * 1024 * 1024 - 1) mustBe "1023 GiB"
    }

    "TiB values" in {
      ByteFormatter.byteCountBinary(1024L * 1024 * 1024 * 1024) mustBe "1 TiB"
      ByteFormatter.byteCountBinary(1024L * 1024 * 1024 * 1024 * 1024 - 1) mustBe "1023 TiB"
    }

    "Extreme values" in {
      ByteFormatter.byteCountBinary(Long.MinValue) mustBe "-8 EiB"
      ByteFormatter.byteCountBinary(Long.MaxValue) mustBe "8 EiB"
    }
  }
}
