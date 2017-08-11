name := "darwin-client"

version := "1.0"

scalaVersion := "2.12.3"

resolvers += Resolver.bintrayRepo("tomverran", "maven")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "com.typesafe.akka" %% "akka-stream" % "2.5.3",
  "io.tvc" %% "akka-streams-stomp" % "0.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
