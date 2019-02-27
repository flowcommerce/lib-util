package io.flow.util

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.{ChronoField, Temporal}
import java.time.{Instant, LocalDate, OffsetDateTime, ZoneId, ZonedDateTime}

object DateHelper {

  object Implicits {
    /** This implicit ordering allows us to called `.sorted` on a Seq[DateTime]. */
    implicit def dateTimeOrdering: Ordering[ZonedDateTime] = Ordering.fromLessThan(_ isBefore _)
  }

  val CopyrightStartYear: Int = 2016

  val EasternTimezone: ZoneId = ZoneId.of("America/New_York")

  val UTCTimeZone: ZoneId = ZoneId.of("UTC")

  /**
    * DateTimeFormatter that emulates the very flexible joda ISODateTimeFormat.dateTimeParser.
    * Should only be used for parsing Strings into TemporalAccessor (Instant/OffsetDateTime/ZonedDateTime)
    * and not for formatting TemporalAccessor to String.
    */
  val ISODateTimeParser = new DateTimeFormatterBuilder().
    parseCaseInsensitive.
    optionalStart.appendValue(ChronoField.YEAR, 4, 19, SignStyle.EXCEEDS_PAD).
    optionalStart.appendLiteral('-').appendValue(ChronoField.MONTH_OF_YEAR, 2).
    optionalStart.appendLiteral('-').appendValue(ChronoField.DAY_OF_MONTH, 2).
    optionalEnd.optionalEnd.optionalEnd.
    optionalStart.appendLiteral('T').appendValue(ChronoField.HOUR_OF_DAY).
    optionalStart.appendLiteral(':').appendValue(ChronoField.MINUTE_OF_HOUR, 2).
    optionalStart.appendLiteral(':').appendValue(ChronoField.SECOND_OF_MINUTE, 2).
    optionalStart.appendLiteral('.').appendFraction(ChronoField.NANO_OF_SECOND, 1, 9, false).
    optionalEnd.optionalEnd.optionalEnd.optionalEnd.
    optionalStart.appendOffsetId.optionalEnd.
    optionalStart.appendLiteral('[').appendZoneId.appendLiteral(']').optionalEnd.
    parseDefaulting(ChronoField.ERA, 1L).
    parseDefaulting(ChronoField.YEAR, 1970L).
    parseDefaulting(ChronoField.MONTH_OF_YEAR, 1L).
    parseDefaulting(ChronoField.DAY_OF_MONTH, 1L).
    parseDefaulting(ChronoField.HOUR_OF_DAY, 0L).
    parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0L).
    parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0L).
    parseDefaulting(ChronoField.NANO_OF_SECOND, 0L).
    parseDefaulting(ChronoField.OFFSET_SECONDS,0L).
    toFormatter

  /**
    * Turns "1" into "01", leaves "12" as "12"
    */
  def prefixZero(value: Int): String = {
    if (value > 0 && value < 10) {
      s"0$value"
    } else {
      value.toString
    }
  }

  def trimLeadingZero(value: String): String = {
    value.stripPrefix("0")
  }

  /**
    * Returns the current year (e.g. 2016) in the eastern timezone
    */
  def currentYear: Int = {
    ZonedDateTime.now(EasternTimezone).getYear
  }

  /**
    * Returns either '2016' or '2016 - 2018' intended to be used for displaying things
    * like the Flow copyright years dynamically.
    */
  def copyrightYears: String = {
    val current = currentYear
    if (current > CopyrightStartYear) {
      s"$CopyrightStartYear - $current"
    } else {
      CopyrightStartYear.toString
    }
  }

  private[this] val filenameDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.
    ofPattern("yyyyMMdd.HHmmss.SSS")

  private[this] val TimeFormat: DateTimeFormatter = DateTimeFormatter.
    ofPattern("HH:mm:ss z")

  def mmmDdYyyy(instant: Instant):  String = mmmDdYyyy(instant.atZone(UTCTimeZone))

  def mmmDdYyyy(offsetDateTime: OffsetDateTime):  String = mmmDdYyyy(offsetDateTime: Temporal)

  def mmmDdYyyy(zonedDateTime: ZonedDateTime):  String = mmmDdYyyy(zonedDateTime: Temporal)

  def mmmDdYyyy(localDate: LocalDate):  String = mmmDdYyyy(localDate: Temporal)

  private def mmmDdYyyy(temporal: Temporal):  String = {
    DateTimeFormatter.ofPattern("MMM d, yyyy").format(temporal)
  }

  def shortDate(instant: Instant):  String = shortDate(instant.atZone(UTCTimeZone))

  def shortDate(offsetDateTime: OffsetDateTime):  String = shortDate(offsetDateTime: Temporal)

  def shortDate(zonedDateTime: ZonedDateTime):  String = shortDate(zonedDateTime: Temporal)

  def shortDate(localDate: LocalDate):  String = shortDate(localDate: Temporal)

  private def shortDate(temporal: Temporal):  String = {
    DateTimeFormatter.ofPattern("d/M/yy").format(temporal)
  }

  def shortDateTime(instant: Instant):  String = shortDateTime(instant.atZone(UTCTimeZone))

  def shortDateTime(offsetDateTime: OffsetDateTime):  String = shortDateTime(offsetDateTime.toZonedDateTime)

  def shortDateTime(zonedDateTime: ZonedDateTime):  String = {
    shortDate(zonedDateTime) + " " + TimeFormat.format(zonedDateTime)
  }

  def longDate(instant: Instant):  String = longDate(instant.atZone(UTCTimeZone))

  def longDate(offsetDateTime: OffsetDateTime):  String = longDate(offsetDateTime: Temporal)

  def longDate(zonedDateTime: ZonedDateTime):  String = longDate(zonedDateTime: Temporal)

  def longDate(localDate: LocalDate):  String = longDate(localDate: Temporal)

  private def longDate(temporal: Temporal):  String = {
    DateTimeFormatter.ofPattern("MMMM d, yyyy").format(temporal)
  }

  def longDateTime(instant: Instant):  String = longDateTime(instant.atZone(UTCTimeZone))

  def longDateTime(offsetDateTime: OffsetDateTime):  String = longDateTime(offsetDateTime.toZonedDateTime)

  def longDateTime(zonedDateTime: ZonedDateTime):  String = {
    longDate(zonedDateTime) + " " + TimeFormat.format(zonedDateTime)
  }

  def consoleLongDateTime(instant: Instant):  String = consoleLongDateTime(instant.atZone(UTCTimeZone))

  def consoleLongDateTime(offsetDateTime: OffsetDateTime):  String = consoleLongDateTime(offsetDateTime.toZonedDateTime)

  def consoleLongDateTime(zonedDateTime: ZonedDateTime):  String = {
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss z").format(zonedDateTime)
  }

  def filenameDateTime(instant: Instant):  String = filenameDateTime(instant.atZone(UTCTimeZone))

  def filenameDateTime(offsetDateTime: OffsetDateTime):  String = filenameDateTime(offsetDateTime.toZonedDateTime)

  def filenameDateTime(zonedDateTime: ZonedDateTime):  String = {
    filenameDateTimeFormatter.format(zonedDateTime)
  }

  /**
    * Returns the specified date (defaults to now) as a string like
    * "201509"
    */
  def yyyymm(instant: Instant):  String = yyyymm(instant.atZone(UTCTimeZone))

  def yyyymm(offsetDateTime: OffsetDateTime):  String = yyyymm(offsetDateTime: Temporal)

  def yyyymm(zonedDateTime: ZonedDateTime):  String = yyyymm(zonedDateTime: Temporal)

  def yyyymm(localDate: LocalDate):  String = yyyymm(localDate: Temporal)

  private def yyyymm(temporal: Temporal):  String = {
    s"${temporal.get(ChronoField.YEAR_OF_ERA)}${DateHelper.prefixZero(temporal.get(ChronoField.MONTH_OF_YEAR))}"
  }

  /**
    * Returns the specified date (defaults to now) as a string like
    * "201509"
    */
  def yyyymmdd(instant: Instant):  String = yyyymmdd(instant.atZone(UTCTimeZone))

  def yyyymmdd(offsetDateTime: OffsetDateTime):  String = yyyymmdd(offsetDateTime: Temporal)

  def yyyymmdd(zonedDateTime: ZonedDateTime):  String = yyyymmdd(zonedDateTime: Temporal)

  def yyyymmdd(localDate: LocalDate):  String = yyyymmdd(localDate: Temporal)

  private def yyyymmdd(temporal: Temporal):  String = {
    yyyymm(temporal) + DateHelper.prefixZero(temporal.get(ChronoField.MONTH_OF_YEAR))
  }

}
