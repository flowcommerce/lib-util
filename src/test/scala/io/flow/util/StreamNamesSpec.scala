package io.flow.util


import io.flow.util.test.v0.models.TestEvent
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class StreamNamesSpec extends AnyWordSpecLike with Matchers {

  private[this] val dev = StreamNames(FlowEnvironment.Development)
  private[this] val ws = StreamNames(FlowEnvironment.Workstation)
  private[this] val prod = StreamNames(FlowEnvironment.Production)

  "validate a service with single name" in {
    dev.json("io.flow.sample.v0.models.Event") must equal(Some("development_workstation.sample.v0.event.json"))
  }

  "production environment is in the stream name" in {
    prod.json("io.flow.sample.v0.models.Event") must equal(Some("production.sample.v0.event.json"))
  }

  "development and workstation environments map to same stream name" in {
    dev.json("io.flow.sample.v0.models.Event") must equal(Some("development_workstation.sample.v0.event.json"))
    ws.json("io.flow.sample.v0.models.Event") must equal(Some("development_workstation.sample.v0.event.json"))
  }

  "validate a service with multi name" in {
    dev.json("io.flow.sample.multi.v0.models.Event") must equal(Some("development_workstation.sample.multi.v0.event.json"))
  }

  "validate a service with long multi name" in {
    dev.json("io.flow.sample.multia.multib.v0.models.Event") must equal(Some("development_workstation.sample.multia.multib.v0.event.json"))
  }

  "invalidate a service with invalid match" in {
    dev.json("io.flow.sample.v0.Event") must be(None)
  }

  "fromType returns proper name for union type" in {
    StreamNames.fromType[TestEvent] must be(
      Right("development_workstation.util.test.v0.test_event.json")
    )
  }

  "fromType returns good error messages" in {
    StreamNames.fromType must be(
      Left(List("FlowKinesisError Stream[Nothing] In order to consume events, you must annotate the type you are expecting as this is used to build the stream. Type should be something like io.flow.user.v0.unions.SomeEvent"))
    )
    StreamNames.fromType[String] must be(
      Left(List("FlowKinesisError Stream[String] Could not parse stream name. Expected something like io.flow.user.v0.unions.SomeEvent"))
    )
  }

  "parse" in {
    StreamNames.parse("io.flow.organization.event.v0.models.OrganizationEvent") must equal(
      Some(
        ApidocClass(
          namespace = "io.flow",
          service = "organization.event",
          version = 0,
          name = "organization_event"
        )
      )
    )

    StreamNames.parse("io.flow.organization.event.v0.models.OrganizationEvent").get.namespaces must equal(
      Seq("organization", "event")
    )
  }

  
}
