name := "darwin-client"

version := "1.0"

scalaVersion := "2.12.3"

resolvers += Resolver.bintrayRepo("tomverran", "maven")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.5.3",
  "io.tvc" %% "akka-streams-stomp" % "0.0.1"
)
