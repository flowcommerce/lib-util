package io.flow.util

import io.flow.util.Config.mustGet
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

/**
  * Wrapper on play config testing for empty strings and standardizing
  * error message for required configuration.
  */
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
  def optionalString(name: String): Option[String] = get(name).map(_.trim) match {
    case Some("") => None
    case v => v
  }

  def requiredString(name: String): String = mustGet(name, optionalString)

  def requiredList(name: String): Seq[String] = mustGet(name, optionalList)

  def optionalLong(name: String): Option[Long] = optionalString(name).map { value =>
    Try(value.toLong) match {
      case Success(v) => v
      case Failure(_) => {
        val msg = s"FlowError Configuration variable[$name] has invalid value[$value]: must be a long"
        logger.error(msg)
        sys.error(msg)
      }
    }
  }

  def requiredLong(name: String): Long = mustGet(name, optionalLong)

  def optionalPositiveLong(name: String): Option[Long] = optionalLong(name) match {
    case None => None
    case Some(v) => if (v > 0) {
      Some(v)
    } else {
      sys.error(s"FlowError Configuration variable[$name] has invalid value[$v]: must be > 0")
    }
  }

  def requiredPositiveLong(name: String): Long = mustGet(name, optionalPositiveLong)

  def optionalInt(name: String): Option[Int] = optionalString(name).map { value =>
    Try(value.toInt) match {
      case Success(v) => v
      case Failure(_) => {
        val msg = s"FlowError Configuration variable[$name] has invalid value[$value]: must be an int"
        logger.error(msg)
        sys.error(msg)
      }
    }
  }

  def requiredInt(name: String): Int = mustGet(name, optionalInt)

  def optionalPositiveInt(name: String): Option[Int] = optionalInt(name) match {
    case None => None
    case Some(v) => if (v > 0) {
      Some(v)
    } else {
      sys.error(s"FlowError Configuration variable[$name] has invalid value[$v]: must be > 0")
    }
  }

  def requiredPositiveInt(name: String): Int = mustGet(name, optionalPositiveInt)

  def optionalBoolean(name: String): Option[Boolean] = optionalString(name).map { value =>
    Booleans.parse(value).getOrElse {
      val msg = s"FlowError Configuration variable[$name] has invalid value[$value]. Use true, t, false, or f"
      logger.error(msg)
      sys.error(msg)
    }
  }

  def requiredBoolean(name: String): Boolean = mustGet(name, optionalBoolean)
}

object Config {
  private def mustGet[T](name: String, valueByName: String => Option[T]): T = {
    valueByName(name).getOrElse {
      sys.error(s"FlowError Configuration variable[$name] is required")
    }
  }
}

case class ChainedConfig(configs: Seq[Config]) extends Config {

  override def optionalList(name: String): Option[Seq[String]] = {
    configs.find { c =>
      c.optionalList(name).isDefined
    }.flatMap(_.optionalList(name))
  }

  override def get(name: String): Option[String] = {
    configs.find { c =>
      c.optionalString(name).isDefined
    }.flatMap(_.optionalString(name))
  }

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
