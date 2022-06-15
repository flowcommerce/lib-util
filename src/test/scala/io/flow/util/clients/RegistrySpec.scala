package io.flow.util.clients

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Future

class RegistrySpec extends AnyWordSpec with Matchers {
  private val registryWithFailingLookup = new ProductionRegistry {
    // outside k8s, the domains don't resolve
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future.failed(new Exception("invalid domain"))
  }
  private val registryWithSucceedingLookup = new ProductionRegistry {
    // in k8s, the domains should all resolve
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future.successful(())
  }
  private val registryWithTimeout = new ProductionRegistry {
    import RegistryEC._
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future(Thread.sleep(1000000))
  }
  private val prodRegistry = new ProductionRegistry()
  private val mock = new MockRegistry()

  val services = Seq("payment", "session", "experience", "experience-jobs")

  "NonK8sProductionRegistry" should {
    services.foreach { service =>
      s"resolve $service" in {
        registryWithFailingLookup.host(service) shouldBe s"https://$service.api.flow.io"
      }
    }
  }

  "K8sProductionRegistry" should {
    services.foreach { service =>
      s"resolve $service" in {
        registryWithSucceedingLookup.host(service) shouldBe s"http://$service"
      }
    }
  }

  "ProductionRegistry with DNS Timeout" should {
    services.foreach { service =>
      s"resolve $service" in {
        registryWithTimeout.host(service) shouldBe s"https://$service.api.flow.io"
      }
    }
  }

  "ProductionRegistry with successful lookup" should {
    s"resolve www.google.com" in {
      prodRegistry.host("www.google.com") shouldBe s"http://www.google.com"
    }
  }

  "MockRegistry" should {
    services.foreach { service =>
      s"resolve $service" in {
        mock.host(service) shouldBe s"http://$service.localhost"
      }
    }
  }
}
