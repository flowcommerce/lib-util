addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.2")

resolvers += "Flow Plugins" at "https://flow.jfrog.io/flow/plugins-release/"
addSbtPlugin("io.flow" % "sbt-flow-linter" % "0.0.55")

addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.11")
