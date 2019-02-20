package io.flow.util

import java.time.{Instant, LocalDate, OffsetDateTime, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter

import org.scalatest.{MustMatchers, WordSpecLike}

class DateHelperSpec extends WordSpecLike with MustMatchers {

  private[this] val jan1OffsetDT: OffsetDateTime = OffsetDateTime.from(DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse("2016-01-01T08:26:18.794-05:00"))

  private[this] val jan1Instant: Instant = jan1OffsetDT.toInstant

  private[this] val jan1ZonedDT: ZonedDateTime = jan1OffsetDT.atZoneSameInstant(DateHelper.EasternTimezone)

  private[this] val jan1LocalDate: LocalDate = jan1OffsetDT.toLocalDate


  "Instant" should {
    "yyyymm" in {
      DateHelper.yyyymm(jan1Instant) must equal("201601")
    }

    "yyyymmdd" in {
      DateHelper.yyyymmdd(jan1Instant) must equal("20160101")
    }

    "mmmDdYyyy" in {
      DateHelper.mmmDdYyyy(jan1Instant) must equal("Jan 1, 2016")
    }

    "shortDate" in {
      DateHelper.shortDate(jan1Instant) must equal("1/1/16")
    }

    "shortDateTime" in {
      DateHelper.shortDateTime(jan1Instant) must equal("1/1/16 13:26:18 UTC")
    }

    "longDate" in {
      DateHelper.longDate(jan1Instant) must equal("January 1, 2016")
    }

    "longDateTime" in {
      DateHelper.longDateTime(jan1Instant) must equal("January 1, 2016 13:26:18 UTC")
    }

    "consoleLongDateTime" in {
      DateHelper.consoleLongDateTime(jan1Instant) must equal("2016-01-01 13:26:18 UTC")
    }

    "currentYear" in {
      DateHelper.currentYear >= 2016
      DateHelper.currentYear <= OffsetDateTime.now.getYear + 1
    }

    "copyrightYear" in {
      val value = DateHelper.copyrightYears
      Seq("2016", s"2016 - ${DateHelper.currentYear}").contains(value) mustBe(true)
    }

    "filenameDateTime" in {
      DateHelper.filenameDateTime(jan1Instant) mustBe "20160101.132618.794"
    }
  }

  "OffsetDateTime" should {
    "yyyymm" in {
      DateHelper.yyyymm(jan1OffsetDT) must equal("201601")
    }

    "yyyymmdd" in {
      DateHelper.yyyymmdd(jan1OffsetDT) must equal("20160101")
    }

    "mmmDdYyyy" in {
      DateHelper.mmmDdYyyy(jan1OffsetDT) must equal("Jan 1, 2016")
    }

    "shortDate" in {
      DateHelper.shortDate(jan1OffsetDT) must equal("1/1/16")
    }

    "shortDateTime" in {
      DateHelper.shortDateTime(jan1OffsetDT) must equal("1/1/16 08:26:18 -05:00")
    }

    "longDate" in {
      DateHelper.longDate(jan1OffsetDT) must equal("January 1, 2016")
    }

    "longDateTime" in {
      DateHelper.longDateTime(jan1OffsetDT) must equal("January 1, 2016 08:26:18 -05:00")
    }

    "consoleLongDateTime" in {
      DateHelper.consoleLongDateTime(jan1OffsetDT) must equal("2016-01-01 08:26:18 -05:00")
    }

    "currentYear" in {
      DateHelper.currentYear >= 2016
      DateHelper.currentYear <= OffsetDateTime.now.getYear + 1
    }

    "copyrightYear" in {
      val value = DateHelper.copyrightYears
      Seq("2016", s"2016 - ${DateHelper.currentYear}").contains(value) mustBe(true)
    }

    "filenameDateTime" in {
      DateHelper.filenameDateTime(jan1OffsetDT) mustBe "20160101.082618.794"
    }
  }

  "ZonedDateTime" should {
    "yyyymm" in {
      DateHelper.yyyymm(jan1ZonedDT) must equal("201601")
    }

    "yyyymmdd" in {
      DateHelper.yyyymmdd(jan1ZonedDT) must equal("20160101")
    }

    "mmmDdYyyy" in {
      DateHelper.mmmDdYyyy(jan1ZonedDT) must equal("Jan 1, 2016")
    }

    "shortDate" in {
      DateHelper.shortDate(jan1ZonedDT) must equal("1/1/16")
    }

    "shortDateTime" in {
      DateHelper.shortDateTime(jan1ZonedDT) must equal("1/1/16 08:26:18 EST")
    }

    "longDate" in {
      DateHelper.longDate(jan1ZonedDT) must equal("January 1, 2016")
    }

    "longDateTime" in {
      DateHelper.longDateTime(jan1ZonedDT) must equal("January 1, 2016 08:26:18 EST")
    }

    "consoleLongDateTime" in {
      DateHelper.consoleLongDateTime(jan1ZonedDT) must equal("2016-01-01 08:26:18 EST")
    }

    "currentYear" in {
      DateHelper.currentYear >= 2016
      DateHelper.currentYear <= OffsetDateTime.now.getYear + 1
    }

    "copyrightYear" in {
      val value = DateHelper.copyrightYears
      Seq("2016", s"2016 - ${DateHelper.currentYear}").contains(value) mustBe(true)
    }

    "filenameDateTime" in {
      DateHelper.filenameDateTime(jan1ZonedDT) mustBe "20160101.082618.794"
      DateHelper.filenameDateTime(jan1ZonedDT.withZoneSameInstant(ZoneOffset.UTC)) mustBe "20160101.132618.794"
    }
  }

  "LocalDate" should {
    "yyyymm" in {
      DateHelper.yyyymm(jan1LocalDate) must equal("201601")
    }

    "yyyymmdd" in {
      DateHelper.yyyymmdd(jan1LocalDate) must equal("20160101")
    }

    "mmmDdYyyy" in {
      DateHelper.mmmDdYyyy(jan1LocalDate) must equal("Jan 1, 2016")
    }

    "shortDate" in {
      DateHelper.shortDate(jan1LocalDate) must equal("1/1/16")
    }

    "longDate" in {
      DateHelper.longDate(jan1LocalDate) must equal("January 1, 2016")
    }

    "currentYear" in {
      DateHelper.currentYear >= 2016
      DateHelper.currentYear <= OffsetDateTime.now.getYear + 1
    }

    "copyrightYear" in {
      val value = DateHelper.copyrightYears
      Seq("2016", s"2016 - ${DateHelper.currentYear}").contains(value) mustBe(true)
    }
  }

  "DateHelper" should {
    "implicit ordering" in {
      val now = OffsetDateTime.now
      val nowPlus1 = now.plusMinutes(1)
      val nowPlus5 = now.plusMinutes(5)
      val nowPlus10 = now.plusMinutes(10)

      val datetimes = Seq(nowPlus10, nowPlus5, nowPlus1, now)

      datetimes.sorted must equal(datetimes.reverse)
    }
  }
}
