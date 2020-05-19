enablePlugins(ScalaJSPlugin)

name := "MiniScala JS"
scalaVersion := "2.13.1"

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0"
