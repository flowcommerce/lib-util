package io.flow.util

import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoField, Temporal}
import java.time.{ZoneId, ZonedDateTime}

object DateHelper {

  object Implicits {
    /** This implicit ordering allows us to called `.sorted` on a Seq[DateTime]. */
    implicit def dateTimeOrdering: Ordering[ZonedDateTime] = Ordering.fromLessThan(_ isBefore _)
  }

  val CopyrightStartYear: Int = 2016

  val EasternTimezone: ZoneId = ZoneId.of("America/New_York")

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

  def mmmDdYyyy(dateTime: Temporal):  String = {
    DateTimeFormatter.ofPattern("MMM d, yyyy").format(dateTime)
  }

  def shortDate(dateTime: Temporal):  String = {
    DateTimeFormatter.ofPattern("d/M/yy").format(dateTime)
  }

  def shortDateTime(dateTime: Temporal):  String = {
    shortDate(dateTime) + " " + TimeFormat.format(dateTime)
  }

  def longDate(dateTime: Temporal):  String = {
    DateTimeFormatter.ofPattern("MMMM d, yyyy").format(dateTime)
  }

  def longDateTime(dateTime: Temporal):  String = {
    longDate(dateTime) + " " + TimeFormat.format(dateTime)
  }

  def consoleLongDateTime(dateTime: Temporal):  String = {
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss z").format(dateTime)
  }

  def filenameDateTime(dateTime: Temporal):  String = {
    filenameDateTimeFormatter.format(dateTime)
  }

  /**
    * Returns the specified date (defaults to now) as a string like
    * "201509"
    */
  def yyyymm(dateTime: Temporal):  String = {
    s"${dateTime.get(ChronoField.YEAR)}${DateHelper.prefixZero(dateTime.get(ChronoField.MONTH_OF_YEAR))}"
  }

  /**
    * Returns the specified date (defaults to now) as a string like
    * "201509"
    */
  def yyyymmdd(dateTime: Temporal):  String = {
    yyyymm(dateTime) + DateHelper.prefixZero(dateTime.get(ChronoField.MONTH_OF_YEAR))
  }

}
