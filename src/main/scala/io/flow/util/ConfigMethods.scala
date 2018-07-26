package io.flow.util

import io.flow.util.ConfigMethods._

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex
import scala.util.{Failure, Try}

/**
  * Concrete Config getters.
  *
  * Methods marked with optionalString return None when the field isn't present.
  * They also throw exceptions if the field's value is present but has an invalid format.
  *
  * Methods marked with `requiredString` follow that behavior, but they also throw if the field isn't present.
  * */
trait ConfigMethods { self: Config =>
  def optionalString(name: String): Option[String] = get(name).map(_.trim).filterNot(_.isEmpty)

  def requiredString(name: String): String = mustGet(name, optionalString)

  def requiredList(name: String): Seq[String] = mustGet(name, optionalList)

  def optionalLong(name: String): Option[Long] = optionalString(name).map { value =>
    Try(value.toLong).getOrElse {
      val msg = s"FlowError Configuration variable[$name] has invalid value[$value]: must be a long"
      logger.error(msg)
      sys.error(msg)
    }
  }

  def requiredLong(name: String): Long = mustGet(name, optionalLong)

  def optionalPositiveLong(name: String): Option[Long] = optionalLong(name).map {
    case v if v > 0 => v
    case v =>
      sys.error(s"FlowError Configuration variable[$name] has invalid value[$v]: must be > 0")
  }

  def requiredPositiveLong(name: String): Long = mustGet(name, optionalPositiveLong)

  def optionalInt(name: String): Option[Int] = optionalString(name).map { value =>
    Try(value.toInt).getOrElse {
      val msg = s"FlowError Configuration variable[$name] has invalid value[$value]: must be an int"
      logger.error(msg)
      sys.error(msg)
    }
  }

  def requiredInt(name: String): Int = mustGet(name, optionalInt)

  def optionalPositiveInt(name: String): Option[Int] = optionalInt(name).map {
    case v if v > 0 => v
    case v =>
      sys.error(s"FlowError Configuration variable[$name] has invalid value[$v]: must be > 0")
  }

  def requiredPositiveInt(name: String): Int = mustGet(name, optionalPositiveInt)

  def optionalFiniteDuration(name: String): Option[FiniteDuration] = {
    def logAndFail(value: String)(cause: Throwable): Nothing = {
      val msg = s"FlowError Configuration variable[$name] has invalid value[$value]. Underlying cause: ${cause.getMessage}"
      logger.error(msg, cause)
      throw new RuntimeException(msg, cause)
    }

    optionalString(name).map { stringValue =>
      duration.parse(stringValue).fold(logAndFail(stringValue), identity)
    }
  }

  def requiredFiniteDuration(name: String): FiniteDuration = mustGet(name, optionalFiniteDuration)

  def optionalBoolean(name: String): Option[Boolean] = optionalString(name).map { value =>
    Booleans.parse(value).getOrElse {
      val msg = s"FlowError Configuration variable[$name] has invalid value[$value]. Use true, t, false, or f"
      logger.error(msg)
      sys.error(msg)
    }
  }

  def requiredBoolean(name: String): Boolean = mustGet(name, optionalBoolean)
}

private object ConfigMethods {
  def mustGet[T](name: String, valueByName: String => Option[T]): T = {
    valueByName(name).getOrElse {
      sys.error(s"FlowError Configuration variable[$name] is required")
    }
  }

  object duration {
    private[duration] val pattern: Regex = """(\d+)\s+(\w+)""".r

    val parse: String => Try[FiniteDuration] = {
      case duration.pattern(amount, unitString) => Try(FiniteDuration(amount.toLong, unitString))
      case _ => Failure(new RuntimeException("Invalid pattern of FiniteDuration"))
    }
  }

}
