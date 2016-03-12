name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.2.11",
  "io.spray" %%  "spray-json" % "1.3.1",
  "net.liftweb" %% "lift-json" % "3.0-M5",
  "com.propensive" %% "rapture-json-jackson" % "1.1.0",
  "com.typesafe.play" %% "play-json" % "2.3.4"
)