name := "lib-util"

organization := "io.flow"

scalaVersion := "2.13.13"
ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")

enablePlugins(GitVersioning)
git.useGitDescribe := true
coverageExcludedFiles := ".*\\/src/main/scala/generated\\/.*"
coverageDataDir := file("target/scala-2.13")
coverageHighlighting := true
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 75
coverageMinimumBranchTotal := 70

lazy val allScalacOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Ypatmat-exhaust-depth",
  "100", // Fixes: Exhaustivity analysis reached max recursion depth, not all missing cases are reported.
  "-Wconf:src=generated/.*:silent",
  "-Wconf:src=target/.*:silent", // silence the unused imports errors generated by the Play Routes
  "-release:17",
)

doc / javacOptions := Seq("-encoding", "UTF-8")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

resolvers += "Artifactory" at "https://flow.jfrog.io/flow/libs-release/"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.12.7", // This is temporary, should use java.time.*
  "org.slf4j" % "slf4j-api" % "1.7.36", // Must follow Play - https://github.com/playframework/playframework/blob/2.8.x/project/Dependencies.scala#L52
  "org.joda" % "joda-convert" % "2.2.3",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "com.github.blemale" %% "scaffeine" % "5.2.1",
  "org.mockito" % "mockito-scala_2.13" % "1.11.3" % Test,
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "org.scalacheck" %% "scalacheck" % "1.18.0" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
)
Test / javaOptions ++= Seq(
  "--add-exports=java.base/sun.security.x509=ALL-UNNAMED",
  "--add-opens=java.base/sun.security.ssl=ALL-UNNAMED",
  "--add-opens=java.base/java.lang=ALL-UNNAMED",
)
scalacOptions ++= allScalacOptions ++ Seq("-release", "17")
scalafmtOnCompile := true
credentials += Credentials(
  "Artifactory Realm",
  "flow.jfrog.io",
  System.getenv("ARTIFACTORY_USERNAME"),
  System.getenv("ARTIFACTORY_PASSWORD"),
)

publishTo := {
  val host = "https://flow.jfrog.io/flow"
  if (isSnapshot.value) {
    Some("Artifactory Realm" at s"$host/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  } else {
    Some("Artifactory Realm" at s"$host/libs-release-local")
  }
}
