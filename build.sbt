import sbt.Keys.{name, version}


lazy val commonSettings = Seq(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  version := "1.0",
  scalaVersion := "2.12.3",
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
//    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
//    "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
  )
)




lazy val algebra = project.in(file("algebra"))
  .settings(commonSettings)
  .settings(
    name := "itinere-algebra",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.2",
      "org.julienrf" %% "enum" % "3.1"
    )
  )

lazy val `json-argonaut` = project.in(file("json-argonaut"))
  .settings(commonSettings)
  .settings(
    name := "itinere-json-argonaut",
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" %% "argonaut-shapeless_6.2" % "1.2.0-M4"
    )
  ).dependsOn(algebra)

lazy val `json-circe` = project.in(file("json-circe"))
  .settings(commonSettings)
  .settings(
    name := "itinere-json-circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.8.0",
      "io.circe" %% "circe-parser" % "0.8.0"
    )
  ).dependsOn(algebra)

lazy val `akka-http-client` = project.in(file("akka-http-client"))
  .settings(commonSettings)
  .settings(
    name := "itinere-akka-http-client",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.0.1"
    )
  ).dependsOn(algebra)


lazy val `akka-http-server` = project.in(file("akka-http-server"))
  .settings(commonSettings)
  .settings(
    name := "itinere-akka-http-server",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.0.1"
    )
  ).dependsOn(algebra)

lazy val http4s = project.in(file("http4s-server"))
  .settings(commonSettings)
  .settings(
    name := "itinere-http4s-server",
    libraryDependencies ++= Seq(
      "org.http4s"     %% "http4s-blaze-server" % "0.17.2"
    )
  ).dependsOn(algebra)


lazy val swagger = project.in(file("swagger"))
  .settings(commonSettings)
  .settings(
    name := "itinere-swagger",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.0-MF"
    )
  ).dependsOn(algebra)

lazy val `swagger-circe` = project.in(file("swagger-circe"))
  .settings(commonSettings)
  .settings(
    name := "itinere-swagger-circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.8.0"
    )
  ).dependsOn(swagger)

lazy val example = project.in(file("example"))
  .settings(commonSettings)
  .settings(libraryDependencies += "io.circe" %% "circe-generic" % "0.8.0")
  .settings(libraryDependencies += "org.webjars" % "swagger-ui" % "3.0.18")
  .dependsOn(`json-circe`, `akka-http-client`, `akka-http-server`, `swagger-circe`, http4s)

lazy val root = (project in file("."))
  .aggregate(algebra, `json-argonaut`, `json-circe`, `akka-http-client`, `akka-http-server`, swagger, `swagger-circe`)
  .settings(
    aggregate in update := false
  )