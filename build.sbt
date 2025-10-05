import sbt.testing.TaskDef

lazy val commonSettings = Seq(
  scalaVersion := "3.6.3",
  scalaJSLinkerConfig ~= {
    _.withModuleKind(ModuleKind.ESModule)
  },
  libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.6.3"
)

lazy val root = (project in file("."))
  .aggregate(
    helloWorld,
    serverInfos
  )

lazy val helloWorld = (project in file("modules/hello-world"))
  .enablePlugins(ScalaJSPlugin, BitburnerPlugin)
  .settings(
    commonSettings
  )
  .dependsOn(BitburnerTypes, common)

lazy val serverInfos = (project in file("modules/server-infos"))
  .enablePlugins(ScalaJSPlugin, BitburnerPlugin)
  .settings(
    name := "server-infos",
    commonSettings
  )
  .dependsOn(BitburnerTypes, common)

lazy val common = (project in file("modules/common"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings)
  .dependsOn(BitburnerTypes)

lazy val BitburnerTypes = (project in file("modules/bitburner-types"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterExternalNpmPlugin)
  .settings(
    commonSettings,
    externalNpm := file(".").getAbsoluteFile(),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.1"
  )
