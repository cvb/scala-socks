name := "socks"

organization := "cvb"

version := "0.0.1"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test" withSources() withJavadoc(),
  "org.typelevel" %% "scodec-core" % "1.5.0",
  "com.typesafe.akka" %% "akka-actor" % "2.3.7"
)

initialCommands := "import cvb.socks._"

