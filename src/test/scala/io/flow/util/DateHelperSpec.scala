package io.flow.util

import java.time.{Instant, LocalDate, OffsetDateTime, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter

import org.scalatest.{MustMatchers, WordSpecLike}

class DateHelperSpec extends WordSpecLike with MustMatchers {

  private[this] val jan1OffsetDT: OffsetDateTime = OffsetDateTime.from(DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse("2016-01-01T08:26:18.794-05:00"))

  private[this] val jan1Instant: Instant = jan1OffsetDT.toInstant

  private[this] val jan1ZonedDT: ZonedDateTime = jan1OffsetDT.atZoneSameInstant(DateHelper.EasternTimezone)

  private[this] val jan1LocalDate: LocalDate = jan1OffsetDT.toLocalDate

  // Java 8 serializes the ZoneId "America/New_York" timezone as "EST", while java 9+ serialize as "GMT-05:00" or "GMT-04:00" depending on daylight saving
  private[this] val ESTZoneId = DateTimeFormatter.ofPattern("z").format(jan1ZonedDT)

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
      DateHelper.shortDateTime(jan1ZonedDT) must equal(s"1/1/16 08:26:18 $ESTZoneId")
    }

    "longDate" in {
      DateHelper.longDate(jan1ZonedDT) must equal("January 1, 2016")
    }

    "longDateTime" in {
      DateHelper.longDateTime(jan1ZonedDT) must equal(s"January 1, 2016 08:26:18 $ESTZoneId")
    }

    "consoleLongDateTime" in {
      DateHelper.consoleLongDateTime(jan1ZonedDT) must equal(s"2016-01-01 08:26:18 $ESTZoneId")
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

    "currentYear" in {
      DateHelper.currentYear >= 2016
      DateHelper.currentYear <= OffsetDateTime.now.getYear + 1
    }

    "copyrightYear" in {
      Seq(DateHelper.copyrightYears) must contain oneOf ("2016", s"2016 - ${DateHelper.currentYear}")
    }
  }

  "IsoDateTime parser" should {
    import DateHelper.ISODateTimeParser
    "All defaults" in {
      ISODateTimeParser.parse("", Instant.from(_)) mustBe Instant.parse("1970-01-01T00:00:00Z")
    }

    "Year only" in {
      ISODateTimeParser.parse("2019", Instant.from(_)) mustBe Instant.parse("2019-01-01T00:00:00Z")
    }

    "Year and month" in {
      ISODateTimeParser.parse("2019-02", Instant.from(_)) mustBe Instant.parse("2019-02-01T00:00:00Z")
    }

    "Year and short month" in {
      ISODateTimeParser.parse("2019-2", Instant.from(_)) mustBe Instant.parse("2019-02-01T00:00:00Z")
    }

    "Year month and day" in {
      ISODateTimeParser.parse("2019-02-26", Instant.from(_)) mustBe Instant.parse("2019-02-26T00:00:00Z")
    }

    "Year short month and short day" in {
      ISODateTimeParser.parse("2019-2-6", Instant.from(_)) mustBe Instant.parse("2019-02-06T00:00:00Z")
    }

    "Hour only" in {
      ISODateTimeParser.parse("T10", Instant.from(_)) mustBe Instant.parse("1970-01-01T10:00:00Z")
    }

    "Short hour only" in {
      ISODateTimeParser.parse("T1", Instant.from(_)) mustBe Instant.parse("1970-01-01T01:00:00Z")
    }

    "Hour and minute" in {
      ISODateTimeParser.parse("T10:15", Instant.from(_)) mustBe Instant.parse("1970-01-01T10:15:00Z")
    }

    "Short hour and short minute" in {
      ISODateTimeParser.parse("T1:5", Instant.from(_)) mustBe Instant.parse("1970-01-01T01:05:00Z")
    }

    "Hour minute and second" in {
      ISODateTimeParser.parse("T10:15:33", Instant.from(_)) mustBe Instant.parse("1970-01-01T10:15:33Z")
    }

    "Short hour short minute and short second" in {
      ISODateTimeParser.parse("T2:5:3", Instant.from(_)) mustBe Instant.parse("1970-01-01T02:05:03Z")
    }

    "Hour minute second and millis" in {
      ISODateTimeParser.parse("T10:15:33.141", Instant.from(_)) mustBe Instant.parse("1970-01-01T10:15:33.141Z")
    }

    "Hour minute second millis and micro" in {
      ISODateTimeParser.parse("T10:15:33.141592", Instant.from(_)) mustBe Instant.parse("1970-01-01T10:15:33.141592Z")
    }

    "Hour minute second millis micro and nano" in {
      ISODateTimeParser.parse("T10:15:33.141592653", Instant.from(_)) mustBe Instant.parse("1970-01-01T10:15:33.141592653Z")
    }

    "Date and hour" in {
      ISODateTimeParser.parse("2019-02-26T10", Instant.from(_)) mustBe Instant.parse("2019-02-26T10:00:00Z")
    }

    "Date hour minute second millis micro and nano" in {
      ISODateTimeParser.parse("2019-02-26T10:15:33.141592653", Instant.from(_)) mustBe Instant.parse("2019-02-26T10:15:33.141592653Z")
    }

    "Date hour and zulu" in {
      ISODateTimeParser.parse("2019-02-26T10Z", Instant.from(_)) mustBe Instant.parse("2019-02-26T10:00:00Z")
    }

    "Date hour minute and zulu" in {
      ISODateTimeParser.parse("2019-02-26T10:15Z", Instant.from(_)) mustBe Instant.parse("2019-02-26T10:15:00Z")
    }

    "Date hour minute second and zulu" in {
      ISODateTimeParser.parse("2019-02-26T10:15:33Z", Instant.from(_)) mustBe Instant.parse("2019-02-26T10:15:33Z")
    }

    "Date time and zulu" in {
      ISODateTimeParser.parse("2019-02-26T10:15:33.141592653Z", Instant.from(_)) mustBe Instant.parse("2019-02-26T10:15:33.141592653Z")
    }

    "Hour and second offset" in {
      ISODateTimeParser.parse("T10+02:10:20", Instant.from(_)) mustBe Instant.parse("1970-01-01T07:49:40Z")
    }

    "Date time and second offset" in {
      ISODateTimeParser.parse("2019-02-26T10:15:33.141592653+02:10:20", Instant.from(_)) mustBe Instant.parse("2019-02-26T08:05:13.141592653Z")
    }

    "Date time and minute offset" in {
      ISODateTimeParser.parse("2019-02-26T10:15:33.141592653+02:10", Instant.from(_)) mustBe Instant.parse("2019-02-26T08:05:33.141592653Z")
    }

    "Hour and minute offset" in {
      ISODateTimeParser.parse("T10+02:10", Instant.from(_)) mustBe Instant.parse("1970-01-01T07:50:00Z")
    }
  }
}
