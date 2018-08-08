package io.flow.util

import org.slf4j.{Logger, LoggerFactory}

/**
  * Wrapper on play config testing for empty strings and standardizing
  * error message for required configuration.
  */
trait Config extends ConfigMethods {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * Return the raw String value for the configuration parameter with the specified name
    */
  protected def get(name: String): Option[String]

  def optionalList(name: String): Option[Seq[String]]

  def optionalMap(name: String): Option[Map[String, Seq[String]]] = None
}

class ChainedConfig(configs: Seq[Config]) extends Config {

  override def get(name: String): Option[String] = optionalFromAny(name, _.optionalString)

  override def optionalList(name: String): Option[Seq[String]] = optionalFromAny(name, _.optionalList)

  private[this] def optionalFromAny[T](name: String, get: Config => String => Option[T]): Option[T] =
    configs.view.flatMap(get(_)(name)).headOption

  override def optionalMap(name: String): Option[Map[String, Seq[String]]] = optionalFromAny(name, _.optionalMap)
}

object ChainedConfig {
  @deprecated("0.0.7", "Use constructor")
  def apply(configs: Seq[Config]): ChainedConfig = new ChainedConfig(configs)
}

object EnvironmentConfig extends EnvironmentConfigLike {
  override protected def source(): Map[String, String] = sys.env
}

trait EnvironmentConfigLike extends Config {
  protected def source(): Map[String, String]

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

  /**
    * Example:
    * Given env:
    * HELLO_WORLD_FOO = "bar, baz"
    * HELLO_WORLD_BAR_BAR = "foo"
    *
    * The call:
    * optionalMap(HELLO_WORLD)
    *
    * will return
    *
    * Map(
    *   "foo" -> Seq("bar", "baz"),
    *   "bar.bar" -> Seq("foo")
    * )
    * */
  override def optionalMap(name: String): Option[Map[String, Seq[String]]] = {
    val prefix = name + "_"

    //////////////////////////////////
    //functions transforming the keys
    //////////////////////////////////
    val stripKey: String => String = _.drop(prefix.length)
    val underscoresAsDots: String => String = _.replaceAllLiterally("_", ".")

    val updateKey: String => String =
      stripKey andThen
        (_.toLowerCase) andThen
        underscoresAsDots

    /////////////////////////////////////////////////////
    //collecting matching keys and transforming the keys
    /////////////////////////////////////////////////////
    val matchingKeys = source().filterKeys(_.startsWith(prefix))

    val transformedMap = matchingKeys.map { case (key, value) =>
      (updateKey(key), value.split(",").map(_.trim).toList)
    }

    if(transformedMap.isEmpty) None
    else Some(transformedMap)
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
