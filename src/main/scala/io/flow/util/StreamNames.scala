package io.flow.util

import scala.reflect._

case class ApidocClass(
                        namespace: String,
                        service: String,
                        version: Int,
                        name: String
                      ) {

  val namespaces: Seq[String] = service.split("\\.")

}

object Naming {

  def dynamoKinesisTableName(streamName: String, appName: String): String = {
    Seq(
      "kinesis",
      streamName,
      appName
    ).mkString(".")
  }

  /**
    * Returns either 'production' or 'development_workstation' based on the
    * flow environment
    */
  def envPrefix(env: FlowEnvironment = FlowEnvironment.Current): String = {
    env match {
      case FlowEnvironment.Production => "production"
      case FlowEnvironment.Development | FlowEnvironment.Workstation => "development_workstation"
    }
  }

}

object StreamNames {

  private[this] val ApidocClassRegexp = """^(io\.flow)\.([a-z]+(\.[a-z]+)*)\.v(\d+)\.models\.(\w+)$""".r

  def parse(name: String): Option[ApidocClass] = {
    name match {
      case ApidocClassRegexp(namespace, service, _, version, n) => {
        Some(
          ApidocClass(
            namespace = namespace,
            service = service,
            version = version.toInt,
            name = toSnakeCase(n)
          )
        )
      }

      case _ => {
        None
      }

    }

  }

  def toSnakeCase(name: String): String = {
    name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
  }

  /**
    * Returns the stream name based on the type of the class (a Right), or a validation
    * error if the class name if invalid (a Left)
    */
  def fromType[T: ClassTag]: Either[Seq[String], String] = {
    val name = classTag[T].toString

    StreamNames(FlowEnvironment.Current).json(name) match {
      case None => {
        name match {
          case "Any" | "Nothing" => {
            Left(Seq(s"FlowKinesisError Stream[$name] In order to consume events, you must annotate the type you are expecting as this is used to build the stream. Type should be something like io.flow.user.v0.unions.SomeEvent"))
          }
          case _ => {
            Left(Seq(s"FlowKinesisError Stream[$name] Could not parse stream name. Expected something like io.flow.user.v0.unions.SomeEvent"))
          }
        }
      }

      case Some(name) => {
        Right(name)
      }
    }
  }
}

case class StreamNames(env: FlowEnvironment) {

  /**
    * Turns a full class name into the name of a kinesis stream
    */
  def json(className: String): Option[String] = {
    StreamNames.parse(className).map { apidoc =>
      s"${Naming.envPrefix(env)}.${apidoc.service}.v${apidoc.version}.${apidoc.name}.json"
    }
  }

}
