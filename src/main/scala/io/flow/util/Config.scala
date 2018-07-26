package io.flow.util

import io.flow.util.Config._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex
import scala.util.{Failure, Try}

/**
  * Wrapper on play config testing for empty strings and standardizing
  * error message for required configuration.
  */
//todo trait AbstractConfig; trait Config extends ConfigConcrete
trait Config {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /*
   * ABSTRACT METHODS
   */

  /**
    * Return the raw String value for the configuration parameter with the specified name
    */
  def get(name: String): Option[String]

  def optionalList(name: String): Option[Seq[String]]

  /*
   * CONCRETE METHODS
   */
  def optionalString(name: String): Option[String] = get(name).map(_.trim).filterNot(_.isEmpty)

  def requiredString(name: String): String = mustGet(name, optionalString)

  def requiredList(name: String): Seq[String] = mustGet(name, optionalList)

  def optionalLong(name: String): Option[Long] = optionalString(name).map { value =>
    Try(value.toLong).getOrElse{
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

private object Config {
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

case class ChainedConfig(configs: Seq[Config]) extends Config {

  override def get(name: String): Option[String] = optionalFromAny(name, _.optionalString)

  override def optionalList(name: String): Option[Seq[String]] = optionalFromAny(name, _.optionalList)

  private[this] def optionalFromAny[T](name: String, get: Config => String => Option[T]): Option[T] =
    configs.view.flatMap(get(_)(name)).headOption
}

object ChainedConfig {
  @deprecated("0.0.7", "Use constructor")
  def apply(configs: Seq[Config]): ChainedConfig = new ChainedConfig(configs)
}

object EnvironmentConfig extends Config {

  override def optionalList(name: String): Option[Seq[String]] = {
    get(name).map { text =>
      text.split(",").map(_.trim)
    }
  }

  override def get(name: String): Option[String] = {
    sys.env.get(name).map(_.trim).map {
      case "" => {
        val msg = s"FlowError Value for environment variable[$name] cannot be blank"
        logger.error(msg)
        sys.error(msg)
      }
      case value => value
    }
  }
}

object PropertyConfig extends Config {

  override def optionalList(name: String): Option[Seq[String]] = {
    get(name).map { text =>
      text.split(",").map(_.trim)
    }
  }

  override def get(name: String): Option[String] = {
    sys.props.get(name).map(_.trim).map {
      case "" => {
        val msg = s"FlowError Value for system property[$name] cannot be blank"
        logger.error(msg)
        sys.error(msg)
      }
      case value => value
    }
  }
}
