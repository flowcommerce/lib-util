package io.flow.util.clients

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Future

class RegistrySpec extends AnyWordSpec with Matchers {
  private val nonk8s = new ProductionRegistry {
    // outside k8s, the domains don't resolve
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future.failed(new Exception("invalid domain"))
  }
  private val k8s = new ProductionRegistry {
    // in k8s, the domains should all resolve
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future.successful(())
  }
  private val k8sWithTimeout = new ProductionRegistry {
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future(Thread.sleep(1000000))
  }
  private val mock = new MockRegistry()

  val services = Seq("payment", "session", "experience", "experience-jobs")

  "NonK8sProductionRegistry" should {
    services.foreach { service =>
      s"resolve $service" in {
        nonk8s.host(service) shouldBe s"https://$service.api.flow.io"
      }
    }
  }

  "K8sProductionRegistry" should {
    services.foreach { service =>
      s"resolve $service" in {
        k8s.host(service) shouldBe s"http://$service"
      }
    }
  }

  "ProductionRegistry with DNS Timeout" should {
    services.foreach { service =>
      s"resolve $service" in {
        k8sWithTimeout.host(service) shouldBe s"https://$service.api.flow.io"
      }
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
