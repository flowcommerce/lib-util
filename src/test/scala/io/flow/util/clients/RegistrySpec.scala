package io.flow.util.clients

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Future

class RegistrySpec extends AnyWordSpec with Matchers {
  private val nonpci = new ProductionRegistry()
  private val k8s = new K8sProductionRegistry {
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future.successful(())
  }
  private val k8sWithTimeout = new K8sProductionRegistry {
    override protected def asyncDnsLookupByName(name: String): Future[Unit] =
      Future(Thread.sleep(1000000))
  }
  private val mock = new MockRegistry()

  "NonK8sProductionRegistry" should {
    "payment" in {
      nonpci.host("payment") shouldBe "https://payment.api.flow.io"
    }
    "session" in {
      nonpci.host("session") shouldBe "https://session.api.flow.io"
    }
    "experience-api" in {
      nonpci.host("experience") shouldBe "https://experience.api.flow.io"
    }
    "experience-jobs" in {
      nonpci.host("experience-jobs") shouldBe "https://experience-jobs.api.flow.io"
    }
  }

  "K8sProductionRegistry" should {
    "payment" in {
      k8s.host("payment") shouldBe "http://payment"
    }
    "session" in {
      k8s.host("session") shouldBe "http://session"
    }
    "experience-api" in {
      k8s.host("experience") shouldBe "http://experience"
    }
    "experience-jobs" in {
      k8s.host("experience-jobs") shouldBe "http://experience-jobs"
    }
  }

  "ProductionRegistry with DNS Timeout" should {
    "payment" in {
      k8sWithTimeout.host("payment") shouldBe "https://payment.api.flow.io"
    }
    "session" in {
      k8sWithTimeout.host("session") shouldBe "https://session.api.flow.io"
    }
    "experience-api" in {
      k8sWithTimeout.host("experience") shouldBe "https://experience.api.flow.io"
    }
    "experience-jobs" in {
      k8sWithTimeout.host("experience-jobs") shouldBe "https://experience-jobs.api.flow.io"
    }
  }

  "MockRegistry" should {
    "payment" in {
      mock.host("payment") shouldBe "http://payment.localhost"
    }
    "session" in {
      mock.host("session") shouldBe "http://session.localhost"
    }
    "experience-api" in {
      mock.host("experience") shouldBe "http://experience.localhost"
    }
    "experience-jobs" in {
      mock.host("experience-jobs") shouldBe "http://experience-jobs.localhost"
    }
  }
}
