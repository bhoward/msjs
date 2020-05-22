enablePlugins(ScalaJSPlugin)

name := "MiniScala JS"
scalaVersion := "2.13.1"

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"
libraryDependencies += "org.scalameta" %%% "scalameta" % "4.3.10"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.2" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")