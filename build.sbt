name := "DgView"

version := "1.0"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.10.3",
  "com.typesafe.akka" %% "akka-actor" % "2.2.3"
)

autoAPIMappings := true

fork := true

outputStrategy := Some(StdoutOutput)
