package io.flow.util.clients

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Future

class K8sProductionRegistry extends ProductionRegistry {
  override def asyncDnsLookupByName(name: String) = Future.successful(())
}

class NonPciProductionRegistry extends ProductionRegistry {
  override def asyncDnsLookupByName(name: String) = Future.failed(new RuntimeException())
}

class DNSTimeoutRegistry extends ProductionRegistry {
  override def asyncDnsLookupByName(name: String) = Future(Thread.sleep(1000000))
}

class RegistrySpec extends AnyWordSpec with Matchers {
  "NonK8sProductionRegistry" should {
    "payment" in {
      new NonPciProductionRegistry().host("payment") shouldBe "https://payment.api.flow.io"
    }
    "session" in {
      new NonPciProductionRegistry().host("session") shouldBe "https://session.api.flow.io"
    }
    "experience-api" in {
      new NonPciProductionRegistry().host("experience") shouldBe "https://experience.api.flow.io"
    }
    "experience-jobs" in {
      new NonPciProductionRegistry().host("experience-jobs") shouldBe "https://experience-jobs.api.flow.io"
    }
  }

  "K8sProductionRegistry" should {
    "payment" in {
      new K8sProductionRegistry().host("payment") shouldBe "http://payment"
    }
    "session" in {
      new K8sProductionRegistry().host("session") shouldBe "http://session"
    }
    "experience-api" in {
      new K8sProductionRegistry().host("experience") shouldBe "http://experience"
    }
    "experience-jobs" in {
      new K8sProductionRegistry().host("experience-jobs") shouldBe "http://experience-jobs"
    }
  }

  "ProductionRegistry with DNS Timeout" should {
    "payment" in {
      new DNSTimeoutRegistry().host("payment") shouldBe "https://payment.api.flow.io"
    }
    "session" in {
      new DNSTimeoutRegistry().host("session") shouldBe "https://session.api.flow.io"
    }
    "experience-api" in {
      new DNSTimeoutRegistry().host("experience") shouldBe "https://experience.api.flow.io"
    }
    "experience-jobs" in {
      new DNSTimeoutRegistry().host("experience-jobs") shouldBe "https://experience-jobs.api.flow.io"
    }
  }

  "MockRegistry" should {
    "payment" in {
      new MockRegistry().host("payment") shouldBe "http://payment.localhost"
    }
    "session" in {
      new MockRegistry().host("session") shouldBe "http://session.localhost"
    }
    "experience-api" in {
      new MockRegistry().host("experience") shouldBe "http://experience.localhost"
    }
    "experience-jobs" in {
      new MockRegistry().host("experience-jobs") shouldBe "http://experience-jobs.localhost"
    }
  }
}
