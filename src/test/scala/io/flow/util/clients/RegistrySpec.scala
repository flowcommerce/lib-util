package io.flow.util.clients

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RegistrySpec extends AnyWordSpec with Matchers {
  "ProductionRegistry" should {
    "payment" in {
      new ProductionRegistry().host("payment") shouldBe "https://payment.api.flow.io"
    }
    "session" in {
      new ProductionRegistry().host("session") shouldBe "https://session.api.flow.io"
    }
    "experience-api" in {
      new ProductionRegistry().host("experience") shouldBe "https://experience.api.flow.io"
    }
    "experience-jobs" in {
      new ProductionRegistry().host("experience-jobs") shouldBe "https://experience-jobs.api.flow.io"
    }
  }

  "K8sProductionRegistry" should {
    "payment" in {
      new K8sProductionRegistry().host("payment") shouldBe "https://payment.api.flow.io"
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
