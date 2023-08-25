package io.flow.util

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Success

private[util] case class AsyncCacheEntry[V](previousValue: Option[V], someExpiresAt: ZonedDateTime, noneExpiresAt: ZonedDateTime, nextValue: Future[Option[V]]) {

  def isExpired: Boolean = {
    val valueToConsider: Option[V] = nextValue.value match {
      case Some(Success(value)) => value
      case _ => previousValue
    }
    val expiry = valueToConsider.fold(noneExpiresAt)(_ => someExpiresAt)
    expiry.isBefore(ZonedDateTime.now())
  }

}

/**
  * Caches data for a short period of time (configurable, defaults to 1 minute)
  *
  * Refreshes data on demand (when you call `get`, if entry is not in cache
  * executes the refresh function then). If the call to `get` fails, and
  * and there is data cached in memory, you will get back the stale data.
  */
trait AsyncCache[K, V] extends Shutdownable {

  private[this] val cache = new java.util.concurrent.ConcurrentHashMap[K, AsyncCacheEntry[V]]()

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

  /**
   * Indicates how to fetch the value for a given key.
   * This function is called when the key is not cached or the value has expired.
   *
   * If the function returns a failed Future and there is a value in the cache, this stale value will be returned.
   */
  def refresh(key: K, firstTime: Boolean): Future[Option[V]]

  def initialContents(): Seq[(K, V)] = Nil


  /**
    * Marks the specified key as expired. On next access, will attempt to refresh. Note that
    * if refresh fails, we will continue to return the stale data.
    */
  def flush(key: K): Unit = {
    cache.computeIfPresent(key, (_: K, entry: AsyncCacheEntry[V]) => {
      val expired = ZonedDateTime.now.minus(1, ChronoUnit.MILLIS)
      entry.copy(someExpiresAt = expired, noneExpiresAt = expired)
    })
    ()
  }

  def get(key: K): Option[V] = {
    // try to do a quick get first
    val finalEntry = Option(cache.get(key)) match {
      case Some(retrievedEntry) if !retrievedEntry.isExpired =>
        retrievedEntry
      case _ =>
        // atomically compute a new entry, to avoid calling "refresh" multiple times
        cache.compute(key, (k: K, currentEntry: AsyncCacheEntry[V]) => {
          Option(currentEntry) match {
            // check again as this value may have been updated by a concurrent call
            case Some(foundEntry) if isShutdown || !foundEntry.isExpired => foundEntry
            case Some(foundEntry) =>
              val previousValue = foundEntry.nextValue.value match {
                case Some(Success(value)) => value
                case _ => foundEntry.previousValue
              }
              doGetEntry(k, previousValue, false)
            case None if isShutdown => AsyncCacheEntry(None, expirationForNone(), expirationForNone(), Future.failed(new RuntimeException("Cache is shut down")))
            case None => doGetEntry(k, None, true)
          }
        })
    }
    finalEntry.nextValue.value match {
      case Some(Success(value)) => value
      case _ => finalEntry.previousValue
    }
  }

  private[this] def doGetEntry(key: K, previousValue: Option[V], firstTime: Boolean): AsyncCacheEntry[V] = {
    AsyncCacheEntry(
      previousValue = previousValue,
      someExpiresAt = expirationForSome(),
      noneExpiresAt = expirationForNone(),
      nextValue = refresh(key, firstTime)
    )
  }

  private[this] def expirationForSome() = {
    ZonedDateTime.now.plusSeconds(durationSeconds)
  }

  private[this] def expirationForNone() = {
    ZonedDateTime.now.plusSeconds(durationForNoneSeconds)
  }

  initialContents().foreach { case (k, v) =>
    val value = Some(v)
    cache.put(k, AsyncCacheEntry(value, expirationForSome(), expirationForSome(), Future.successful(value)))
  }
}
