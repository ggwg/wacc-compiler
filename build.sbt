name := "wacc_41"
version := "0.1"
scalaVersion := "2.13.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

libraryDependencies += "com.github.j-mie6" %% "parsley" % "2.7.0"

mainClass in (Compile, run) := Some("Compiler")

mainClass in assembly := Some("Compiler")

test in assembly := {}
