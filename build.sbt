name := "mango"

version := "0.1"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-optimise")



libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.1"

libraryDependencies += "com.esyfur" % "rql" % "0.1.+"


// fork in run := true

// javaOptions in run += "-agentlib:hprof=heap=sites,cpu=times,lineno=y,depth=16"
