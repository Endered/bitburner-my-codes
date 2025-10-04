lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.6.3"
  )

lazy val helloWorld = (project in file("hello-world"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .settings(
    scalaVersion := "3.6.3",
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    externalNpm := (root / baseDirectory).value,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.1"
  )
