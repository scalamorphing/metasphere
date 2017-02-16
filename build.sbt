scalaVersion in ThisBuild := "2.11.8"

lazy val metasphere = {
  project in file(".")
}.enablePlugins(
  ScalaJSPlugin
).settings(
  name := "metashpere",
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  version := "0.0.1",
  organization := "expert.scalamorphing",
  scalaSource in Compile := file(s"${baseDirectory.value.getAbsolutePath}/bundle"),
  scalaSource in Test := file(s"${baseDirectory.value.getAbsolutePath}/bundle"),
  libraryDependencies += "org.scalactic" %%% "scalactic" % "3.0.1" % "test",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test",
  crossPaths := false,
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2",
  libraryDependencies += "io.monix" %%% "monix" % "2.2.1",
  libraryDependencies += "io.monix" %%% "monix-cats" % "2.2.1",
  libraryDependencies += "expert.scalamorphing" %%% "slowparser" % "0.0.1",
  resolvers += "bintray" at "https://dl.bintray.com/scalamorphing/maven",
  libraryDependencies += "expert.scalamorphing" %%% "slowparser" % "0.0.1"
)
