scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.8",
  "io.circe" %% "circe-core" % "0.8.0",
  "org.scalaj" %% "scalaj-http" % "2.3.0",
  "org.zeroturnaround" % "zt-zip" % "1.12",
  "org.slf4j" % "slf4j-simple" % "1.7.25"
)

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
