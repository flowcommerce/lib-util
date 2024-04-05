package io.flow.util.clients

import io.flow.util.{ChainedConfig, Config, EnvironmentConfig, FlowEnvironment, PropertyConfig}
import org.slf4j.{Logger, LoggerFactory}

import java.net.InetAddress
import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.Try

/** This class implements service discovery for flow based on the environment in which we are in. In production,
  * hostnames are build using convention (e.g. 'user' => 'user.api.flow.io'). In development, hostnames are built by
  * querying the registry for port mappings.
  *
  * Example:
  *
  * lazy val client = new Client(new Registry(env).host("user"))
  */
trait Registry {

  /** Returns the host of the application (e.g. http://user.api.flow.io or http://vm:6011)
    */
  def host(applicationId: String): String

}

object RegistryConstants {
  private[clients] implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newWorkStealingPool(6))

  private[clients] val DnsLookupWaitTime = 100.millis

  private val logger: Logger = {
    val name = getClass.getName.reverse.dropWhile(_ == '$').reverse
    LoggerFactory.getLogger(name)
  }

  private val config: Config = ChainedConfig(EnvironmentConfig, PropertyConfig)

  val ProductionDomain = "api.flow.io"

  val WorkstationHostVariableName = "WORKSTATION_HOST"

  val DisableInternalLookup = "DISABLE_INTERNAL_HOSTNAME_LOOKUP"

  val DefaultWorkstationHost = "ws"

  /** Defaults to the workstation host
    */
  private[this] lazy val devHost: String = workstationHost

  /** The resolved name of the host used in workstation
    */
  private[this] lazy val workstationHost: String = {
    config.optionalString(WorkstationHostVariableName).getOrElse {
      logger.info(
        s"${logger.getName}: defaulting workstationHost to '$DefaultWorkstationHost' (can override via env var[$WorkstationHostVariableName])",
      )
      DefaultWorkstationHost
    }
  }

  private[clients] lazy val internalLookupDisabled =
    config.optionalBoolean(RegistryConstants.DisableInternalLookup).getOrElse(false)

  def log(env: String, applicationId: String, message: String): Unit = {
    logger.info(s"${logger.getName}: env[$env] app[$applicationId] $message")
  }

  /** Returns the public hostname of the specified application in the production environment.
    */
  def productionHost(applicationId: String): String = {
    s"https://$applicationId.$ProductionDomain"
  }

  def developmentHost(port: Long): String = {
    s"http://$devHost:$port"
  }

  def workstationHost(port: Long): String = {
    s"http://$workstationHost:$port"
  }

  def host(applicationId: String, port: Long): String = {
    FlowEnvironment.Current match {
      case FlowEnvironment.Production => productionHost(applicationId)
      case FlowEnvironment.Development => developmentHost(port)
      case FlowEnvironment.Workstation => workstationHost(port)
    }
  }
}

/** Production works by convention with no external dependencies.
  */
class ProductionRegistry() extends Registry {
  import RegistryConstants.ec

  override def host(applicationId: String): String = {
    val publicHost = RegistryConstants.productionHost(applicationId)

    val host = if (RegistryConstants.internalLookupDisabled) {
      publicHost
    } else {
      Try {
        Await.result(asyncDnsLookupByName(applicationId), RegistryConstants.DnsLookupWaitTime)
      }.fold(_ => publicHost, _ => s"http://$applicationId")
    }

    RegistryConstants.log("Production", applicationId, s"host[$host]")
    host
  }

  protected def asyncDnsLookupByName(name: String): Future[Unit] = {
    Future {
      InetAddress.getByName(name)
    }.map(_ => ())
  }
}

class MockRegistry() extends Registry {
  override def host(applicationId: String) = s"http://$applicationId.localhost"
}
