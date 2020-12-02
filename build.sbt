name := "ListsBeleg"

version := "0.1"

scalaVersion := "2.13.4"
scalacOptions in ThisBuild ++= Seq("-deprecation")
libraryDependencies ++=Seq("org.scalactic" %% "scalactic" % "3.1.1" % "test",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"	)