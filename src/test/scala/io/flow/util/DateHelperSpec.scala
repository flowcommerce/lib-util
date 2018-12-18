package io.flow.util

import java.time.{OffsetDateTime, ZoneId, ZoneOffset}
import java.time.format.DateTimeFormatter

import org.scalatest.{MustMatchers, WordSpecLike}

class DateHelperSpec extends WordSpecLike with MustMatchers {

  private[this] val jan1 = OffsetDateTime.from(DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse("2016-01-01T08:26:18.794-05:00"))
    .atZoneSameInstant(DateHelper.EasternTimezone)

  "yyyymm" in {
    DateHelper.yyyymm(jan1) must equal("201601")
  }

  "yyyymmdd" in {
    DateHelper.yyyymmdd(jan1) must equal("20160101")
  }

  "mmmDdYyyy" in {
    DateHelper.mmmDdYyyy(jan1) must equal("Jan 1, 2016")
  }

  "shortDate" in {
    DateHelper.shortDate(jan1) must equal("1/1/16")
  }

  "shortDateTime" in {
    DateHelper.shortDateTime(jan1) must equal("1/1/16 08:26:18 EST")
  }

  "longDate" in {
    DateHelper.longDate(jan1) must equal("January 1, 2016")
  }

  "longDateTime" in {
    DateHelper.longDateTime(jan1) must equal("January 1, 2016 08:26:18 EST")
  }

  "consoleLongDateTime" in {
    DateHelper.consoleLongDateTime(jan1) must equal("2016-01-01 08:26:18 EST")
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
    DateHelper.filenameDateTime(jan1.withZoneSameInstant(ZoneId.of("America/New_York"))) mustBe "20160101.082618.794"
    DateHelper.filenameDateTime(jan1.withZoneSameInstant(ZoneOffset.UTC)) mustBe "20160101.132618.794"
  }

  "implicit ordering" in {
    val now = OffsetDateTime.now
    val nowPlus1 = now.plusMinutes(1)
    val nowPlus5 = now.plusMinutes(5)
    val nowPlus10 = now.plusMinutes(10)

    val datetimes = Seq(nowPlus10, nowPlus5, nowPlus1, now)

    datetimes.sorted must equal(datetimes.reverse)
  }

}
