import NativePackagerKeys._

name := "DgView"

version := "1.0"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _)

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"

autoAPIMappings := true

fork := true

outputStrategy := Some(StdoutOutput)

packageArchetype.java_application

packageSummary := "Spectral element visualizer"

packageDescription := "Visualize spectral element data while maintaining a responsive UI."

maintainer := "Curran D. Muhlberger <curran@muhlbergerweb.com>"
