name := "lib-util"
organization := "io.flow"

scalaVersion := "2.12.6"

javacOptions in doc := Seq("-encoding", "UTF-8")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

resolvers += "Artifactory" at "https://flow.jfrog.io/flow/libs-release/"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "joda-time" % "joda-time" % "2.9.9", // This is temporary, should use java.time.*
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.mockito" % "mockito-all" % "1.10.19" % Test,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

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

version := "0.1.0-SNAPSHOT"
