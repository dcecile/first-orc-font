scalaVersion := "2.12.3"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := false

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import"
)

scalaSource in Compile := baseDirectory.value / "src"
