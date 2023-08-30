package io.flow.util

import com.github.blemale.scaffeine.{LoadingCache, Scaffeine}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.nowarn
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Caches data for a short period of time (configurable, defaults to 1 minute)
  *
  * Refreshes data on demand (when you call `get`, if entry is not in cache
  * executes the refresh function then). If the call to `get` fails, and
  * and there is data cached in memory, you will get back the stale data.
  */
trait CacheWithFallbackToStaleData[K, V] extends Shutdownable {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private[this] val cache: LoadingCache[K, V] = Scaffeine()
    .refreshAfterWrite(refreshInterval)
    .expireAfter(
      create = computeExpiry,
      update = (k: K, v: V, _: FiniteDuration) => computeExpiry(k, v),
      read = (_: K, _: V, d: FiniteDuration) => d,
    )
    .build[K, V](
      loader = refreshInternal _
    )

  private[this]def bootstrap() = {
    Try(
      cache.putAll(initialContents().toMap)
    ) match {
      case Success(_) => ()
      case Failure(f) => logger.warn("Failed to bootstrap cache", f)
    }
  }

  bootstrap()

  @deprecated("Use refreshInterval", "0.2.21")
  def duration: FiniteDuration = 1.minute

  /**
   * Defines the duration after which values are reloaded in the background
   */
  @nowarn("cat=deprecation")
  def refreshInterval: FiniteDuration = duration

  @deprecated("Use expireNoneInterval", "0.2.21")
  def durationForNone: FiniteDuration = 2.seconds

  /**
   * Defines the duration after which `None` values are removed from the cache.
   * A common use case is caching the lookup of an item - this allows us
   * to more quickly check if that item is now defined which is a common case
   * when consuming events. Defaults to 2 seconds.
   */
  @nowarn("cat=deprecation")
  def expireNoneInterval: FiniteDuration = durationForNone

  /**
   * Defines the duration after which values (that are not `None`) are removed from the cache.
   * If keys are looked up, they never expire and instead get refreshed in the background.
   * Only if they are not looked up within the expire interval, they are removed.
   * Defaults to 5 times the refresh interval.
   */
  def expireInterval: FiniteDuration = 5 * refreshInterval

  /**
   * Can be used to populate the cache with an initial key-value set, instead of starting empty
   * @return The initial keys and values
   */
  protected def initialContents(): Seq[(K, V)] = Nil

  /**
   * Indicates how to fetch the value for a given key.
   * This function is called when the key is not cached or the value has expired.
   *
   * If the function throws an exception and there is a value in the cache, this stale value will be returned.
   * Otherwise the exception is thrown and must be handled.
   * @see [[safeGet]] and [[getOrElse]]
   */
  protected def refresh(key: K): V

  /**
    * Removes the specified key. On next access, will attempt to load it again. Note that
    * if refresh fails, the Exception is passed to the caller.
    */
  def flush(key: K): Unit = {
    cache.underlying.invalidate(key)
  }

  /**
   * Forces an asynchronous refresh of the specified key. If the key was present before, the cache will continue
   * to return the previous value until the Future has completed.
   *
   * @return A Future of the new value
   */
  def forceRefresh(key: K): Future[V] = cache.refresh(key)

  /**
   * Looks up the cache value for the specified key. If the key is not already in the cache this will synchronously
   * invoke `refresh`, block until `refresh` has returned and may throw an Exception when the loading of the key fails.
   * If the key is already present it will return the current value, regardless of the refresh interval of the key.
   * If the key's refresh interval has passed, it will asynchronously attempt to refresh the key, and update the cache.
   */
  def get(key: K): V = cache.get(key)

  /**
   * Looks up the cache value for the specified key, and returns it if it is already present.
   * If the key is not already in the cache this will asynchronously invoke `refresh`, and add it to the cache.
   */
  def getIfPresent(key: K): Option[V] = cache.getIfPresent(key).orElse {
    forceRefresh(key)
    None
  }

  /**
   * Use instead of [[get]] if [[refresh]] may throw an exception.
   * @see [[getOrElse]] to return a default value
   */
  def safeGet(key: K): Try[V] = Try(get(key))

  /**
   * Returns the `default` value if the underlying call to [[get]] throws an exception
   */
  def getOrElse(key: K, default: => V): V = safeGet(key).getOrElse(default)

  private def refreshInternal(key: K): V = {
    if (isShutdown)
      throw new RuntimeException("This cache has been shutdown")
    else
      refresh(key)
  }

  @nowarn("cat=unused")
  private def computeExpiry(k: K, v: V): FiniteDuration = v match {
    case option: Option[_] =>
      option.fold(expireNoneInterval)(_ => expireInterval)
    case _ =>
      expireInterval
  }
}
