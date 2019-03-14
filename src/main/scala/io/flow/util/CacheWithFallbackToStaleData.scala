package io.flow.util

import java.time.ZonedDateTime
import java.time.temporal.ChronoField

import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

private[util] case class CacheEntry[V](value: V, expiresAt: ZonedDateTime) {

  def isExpired: Boolean = expiresAt.isBefore(ZonedDateTime.now())

}

/**
  * Caches data for a short period of time (configurable, defaults to 1 minute)
  *
  * Refreshes data on demand (when you call `get`, if entry is not in cache
  * executes the refresh function then). If the call to `get` failes, and
  * and there is data cached in memory, you will get back the stale data.
  */
trait CacheWithFallbackToStaleData[K, V] {

  private[this] val cache = new java.util.concurrent.ConcurrentHashMap[K, CacheEntry[V]]()
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * Defines how long to cache each value for
    */
  def duration: FiniteDuration = FiniteDuration(1, MINUTES)

  private[this] val DefaultDurationSecondsForNone = 2L

  /**
    * If the result of the operation is either None, defines
    * how long we cache this value for. A common use case at
    * Flow is caching the lookup of an item - this allows us
    * to more quickly check if that item is now defined which
    * is a common case when consuming events. Defaults to
    * the lower of the overall cache duration or 2 seconds.
    */
  def durationForNone: FiniteDuration = FiniteDuration(
    Seq(DefaultDurationSecondsForNone, duration.toSeconds).min,
    SECONDS
  )

  private[this] lazy val durationSeconds = duration.toSeconds
  private[this] lazy val durationForNoneSeconds = durationForNone.toSeconds

  def refresh(key: K): V

  /**
    * Marks the specified key as expired. On next access, will attempt to refresh. Note that
    * if refresh fails, we will continue to return the stale data.
    */
  def flush(key: K): Unit = {
    cache.computeIfPresent(key, (_: K, entry: CacheEntry[V]) => {
      entry.copy(expiresAt = ZonedDateTime.now.minus(1, ChronoField.MILLI_OF_DAY.getBaseUnit))
    })
    ()
  }

  def get(key: K): V = {
    // try to do a quick get first
    val finalEntry = Option(cache.get(key)) match {
      case Some(retrievedEntry) =>
        if (!retrievedEntry.isExpired) retrievedEntry
        else {
          // atomically compute a new entry, to avoid calling "refresh" multiple times
          cache.compute(key, (k: K, currentEntry: CacheEntry[V]) => {
            Option(currentEntry) match {
              // check again as this value may have been updated by a concurrent call
              case Some(foundEntry) =>
                if (!foundEntry.isExpired) foundEntry
                else doGetEntry(k)(failureFromRefresh(k, foundEntry, _))
              case None => doGetEntry(k)(failureFromEmpty(k, _))
            }
          })
        }
      // compute if absent as this value may have been updated by a concurrent call
      case None => cache.computeIfAbsent(key, (k: K) => doGetEntry(k)(failureFromEmpty(k, _)))
    }
    finalEntry.value
  }

  private[this] def failureFromEmpty(key: K, ex: Throwable): CacheEntry[V] = {
    val msg = s"FlowError for Cache[${this.getClass.getName}] key[$key]: ${ex.getMessage}"
    logger.error(msg, ex)
    sys.error(msg)
  }

  private[this] def failureFromRefresh(key: K, currentEntry: CacheEntry[V], ex: Throwable): CacheEntry[V] = {
    logger.warn(s"Cache[${this.getClass.getName}] key[$key]: Falling back to stale data " +
      s"as refresh failed with: ${ex.getMessage}", ex)
    currentEntry
  }

  private[this] def doGetEntry(key: K)(failureFunction: Throwable => CacheEntry[V]): CacheEntry[V] = {
    Try(refresh(key)) match {
      case Success(value) => {
        CacheEntry(
          value = value,
          expiresAt = ZonedDateTime.now.plusSeconds(expirationInSeconds(value))
        )
      }
      case Failure(ex) => failureFunction(ex)
    }
  }

  private[this] def expirationInSeconds(value: V): Long = {
    value match {
      case option: Option[_] =>
        option.fold(durationForNoneSeconds)(_ => durationSeconds)
      case _ =>
        durationSeconds
    }
  }
}
