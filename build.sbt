name := "itinere"

version := "1.0"

scalaVersion := "2.12.3"



libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.typesafe.akka" %% "akka-http" % "10.0.1"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
