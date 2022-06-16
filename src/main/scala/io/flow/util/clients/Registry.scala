package io.flow.util.clients

import io.flow.util.{EnvironmentConfig, FlowEnvironment, PropertyConfig}
import org.slf4j.{Logger, LoggerFactory}

import java.net.InetAddress
import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.Try

/**
  * This class implements service discovery for flow based on the
  * environment in which we are in. In production, hostnames are build
  * using convention (e.g. 'user' => 'user.api.flow.io'). In
  * development, hostnames are built by querying the registry for port
  * mappings.
  *
  * Example:
  *
  *    lazy val client = new Client(new Registry(env).host("user"))
  */
trait Registry {

  /**
    * Returns the host of the application
    * (e.g. http://user.api.flow.io or http://vm:6011)
    */
  def host(applicationId: String): String

}

private[clients] object RegistryEC {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newWorkStealingPool())
}

trait RegistryConstants {
  import RegistryEC._

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  val ProductionDomain = "api.flow.io"

  val WorkstationHostVariableName = "WORKSTATION_HOST"

  val DefaultWorkstationHost = "ws"

  protected val DnsLookupWaitTime = 100.millis

  /**
    * Defaults to the workstation host
    */
  private[this] lazy val devHost: String = workstationHost

  /**
    * The resolved name of the host used in workstation
    */
  private[this] lazy val workstationHost: String = {
    EnvironmentConfig.optionalString(WorkstationHostVariableName).getOrElse {
      PropertyConfig.optionalString(WorkstationHostVariableName).getOrElse {
        logger.info(s"[${getClass.getName}] defaulting workstationHost to '$DefaultWorkstationHost' (can override via env var[$WorkstationHostVariableName])")
        DefaultWorkstationHost
      }
    }
  }

  def log(env: String, applicationId: String, message: String): Unit = {
    logger.info(s"[${getClass.getName} $env] app[$applicationId] $message")
  }

  /**
    * Returns the public hostname of the specified application in the
    * production environment.
    */
  def productionHost(applicationId: String): String = {
    Try {
      Await.result(asyncDnsLookupByName(applicationId), DnsLookupWaitTime)
    }.fold(_ => s"https://$applicationId.$ProductionDomain", _ => s"http://$applicationId")
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

  protected def asyncDnsLookupByName(name: String): Future[Unit] = {
    Future {
      val addr = InetAddress.getByName(name)
      println(s"addr=$addr")
    }.map(_ => ())
  }
}

object RegistryConstants extends RegistryConstants

/**
  * Production works by convention with no external dependencies.
  */
class ProductionRegistry() extends Registry with RegistryConstants {
  override def host(applicationId: String): String = {
    val host = productionHost(applicationId)
    log("Production", applicationId, s"Host[$host]")
    host
  }

}

class MockRegistry() extends Registry {
  override def host(applicationId: String) = s"http://$applicationId.localhost"
}
