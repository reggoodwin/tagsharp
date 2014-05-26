name := "tagsharp"

version := "1.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.2"

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
