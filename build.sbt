lazy val root = (project in file("."))
  .aggregate(
    helloWorld
  )

lazy val helloWorld = (project in file("hello-world"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.6.3",
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    }
  )
  .dependsOn(BitburnerTypes)

lazy val BitburnerTypes = (project in file("bitburner-types"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .settings(
    scalaVersion := "3.6.3",
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    externalNpm := file(".").getAbsoluteFile(),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.1"
  )
