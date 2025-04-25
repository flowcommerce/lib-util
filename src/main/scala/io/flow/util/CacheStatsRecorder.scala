package io.flow.util

/** A structural type indicating there is a stats counter available.
  */
trait HasCacheStatsRecorder {
  def cacheStatsRecorder: CacheStatsRecorder
}

trait NoOpCacheStatsRecorder extends HasCacheStatsRecorder {
  override def cacheStatsRecorder: CacheStatsRecorder = CacheStatsRecorder.NoOpCacheStatsRecorder
}

trait CacheStatsRecorder {
  def recordHits(count: Long): Unit
  def recordMisses(count: Long): Unit
  def recordLoadSuccess(loadTimeNanos: Long): Unit
  def recordLoadFailure(loadTimeNanos: Long): Unit
  def recordRemoval(reason: CacheStatsRecorder.RemovalReason): Unit
}

object CacheStatsRecorder {
  case object NoOpCacheStatsRecorder extends CacheStatsRecorder {
    override def recordHits(count: Long): Unit = ()
    override def recordMisses(count: Long): Unit = ()
    override def recordLoadSuccess(loadTimeNanos: Long): Unit = ()
    override def recordLoadFailure(loadTimeNanos: Long): Unit = ()
    override def recordRemoval(reason: RemovalReason): Unit = ()
  }

  sealed trait RemovalReason
  object RemovalReason {
    case object Explicit extends RemovalReason {
      override def toString: String = "explicit"
    }

    case object Replaced extends RemovalReason {
      override def toString: String = "replaced"
    }

    case object Collected extends RemovalReason {
      override def toString: String = "collected"
    }

    case object Expired extends RemovalReason {
      override def toString: String = "expired"
    }

    case object SizeConstraint extends RemovalReason {
      override def toString: String = "size_constraint"
    }
  }
}
