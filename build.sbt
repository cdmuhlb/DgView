import NativePackagerKeys._

name := "DgView"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "org.scalanlp" %% "breeze" % "0.5.2"
)

autoAPIMappings := true

fork := true

outputStrategy := Some(StdoutOutput)

packageArchetype.java_application

packageSummary := "Spectral element visualizer"

packageDescription := "Visualize spectral element data while maintaining a responsive UI."

maintainer := "Curran D. Muhlberger <curran@muhlbergerweb.com>"
