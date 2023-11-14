package io.flow.util

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class FlowEnvironmentSpec extends AnyWordSpecLike with Matchers {

  "fromString" in {
    FlowEnvironment.fromString("development") must be(Some(FlowEnvironment.Development)): Unit
    FlowEnvironment.fromString("production") must be(Some(FlowEnvironment.Production)): Unit
    FlowEnvironment.fromString("workstation") must be(Some(FlowEnvironment.Workstation))
  }

  "Current is defined" in {
    FlowEnvironment.all.contains(FlowEnvironment.Current) must be(true)
  }

  "parse" in {
    FlowEnvironment.parse("test", "development") must be(FlowEnvironment.Development): Unit
    FlowEnvironment.parse("test", "production") must be(FlowEnvironment.Production): Unit
    intercept[Throwable] {
      FlowEnvironment.parse("test", "other")
    }.getMessage must be(
      "Value[other] from test[FLOW_ENV] is invalid. Valid values are: development, production, workstation",
    )
  }

}
