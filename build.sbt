name := "prefuse-type-debugger"

organization := "EPFL"

version := "0.0.4"

resolvers += "snapshots" at 
"https://oss.sonatype.org/content/repositories/snapshots/" 

// pick local version of scala compiler & library

scalaHome <<= baseDirectory { f =>
  val props = new java.util.Properties()
  IO.load(props, f / "local.properties")
  val x = props.getProperty("scala.instrumented.home")
  if (x == null)
    sys.error("I need scala compiler version with instrumentation on. Define scala.instrumented.home in local.properties")
  else {
    println("Using: " + x)
    Some(file(x))
  }
}

unmanagedJars in Compile <++= (scalaHome, baseDirectory) map { (sHome, base) =>
  val scalaCompiler = (sHome.get / "lib" / "scala-compiler.jar")
  val unmanagedDirs = base +++ (base / "lib")
  val allJars = (unmanagedDirs ** ".jars") +++ scalaCompiler
  allJars.classpath
}


scalaVersion := "2.10.0-SNAPSHOT"

resolvers ++= Seq(ScalaToolsSnapshots)

