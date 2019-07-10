name := "lib-util"
organization := "io.flow"

scalaVersion := "2.12.8"

javacOptions in doc := Seq("-encoding", "UTF-8")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

resolvers += "Artifactory" at "https://flow.jfrog.io/flow/libs-release/"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "joda-time" % "joda-time" % "2.10.1", // This is temporary, should use java.time.*
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.mockito" % "mockito-all" % "1.10.19" % Test,
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.3.1"),
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "com.github.ghik" %% "silencer-lib" % "1.3.0" % Provided
)

// silence all warnings on autogenerated files
flowGeneratedFiles ++= Seq(
  "src/main/scala/io/flow/common/v0/models/.*".r,
)

// Make sure you only exclude warnings for the project directories, i.e. make builds reproducible
scalacOptions += s"-P:silencer:sourceRoots=${baseDirectory.value.getCanonicalPath}"

credentials += Credentials(
  "Artifactory Realm",
  "flow.jfrog.io",
  System.getenv("ARTIFACTORY_USERNAME"),
  System.getenv("ARTIFACTORY_PASSWORD")
)

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}

version := "0.1.28"
