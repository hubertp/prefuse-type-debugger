import sbt._
import Keys._

object TypeDebugger extends Build {
  val tdsettings = Defaults.defaultSettings ++ Seq(
    organization := "EPFL",
    name         := "prefuse-type-debugger",
    version      := "0.0.4",
    scalaVersion := "2.10.0-SNAPSHOT", 
    scalacOptions in Compile += "-unchecked",
    resolvers ++= Seq("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")
    //logLevel     := Level.Debug
  )
  
  lazy val localScala = scalaHome <<= baseDirectory { f =>
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
  
  lazy val jars = unmanagedJars in Compile <++= (scalaHome, baseDirectory) map { (sHome, base) =>
    val scalaCompiler = (sHome.get / "lib" / "scala-compiler.jar")
    val unmanagedDirs = base +++ (base / "lib")
    val allJars = (unmanagedDirs ** ".jars") +++ scalaCompiler
    allJars.classpath
  }
  
  lazy val forkExtra = fork in ExtraTasks:= true // in order to add scala-library.jar & scala-compiler.jar to bootclasspath
  
  
  // -----------------------------------------
  // copied from sbt.Defaults because I needed 'fork' to be scoped properly for running
  def myFullRunInputTask(scoped: InputKey[Unit], config: Configuration, mainClass: String, baseArguments: String*): Setting[InputTask[Unit]] =
  scoped <<= inputTask { result =>
    ( initScoped(scoped.scopedKey, myRunnerInit(config)) zipWith (fullClasspath in config, streams, result).identityMap) { (rTask, t) =>
      (t :^: rTask :^: KNil) map { case (cp, s, args) :+: r :+: HNil =>
        toError(r.run(mainClass, sbt.Build.data(cp), baseArguments ++ args, s.log))
      }
    }
  }
  
  def myRunnerInit(scope: Configuration): sbt.Project.Initialize[Task[ScalaRun]] =
    (taskTemporaryDirectory, scalaInstance, baseDirectory, javaOptions, outputStrategy, fork in scope, javaHome, trapExit, connectInput) map {
      (tmp, si, base, options, strategy, forkRun, javaHomeDir, trap, connectIn) =>
        if(forkRun) {
          new ForkRun( ForkOptions(scalaJars = si.jars, javaHome = javaHomeDir, connectInput = connectIn, outputStrategy = strategy, runJVMOptions = options, workingDirectory = Some(base)) )
        } else
          new Run(si, trap, tmp)
    }
  // -----------------------------------------
  
  def allResources = TaskKey[List[Seq[String]]]("all-resources", "Gets the list of all valiable resources")
  def allResourcesTask: Setting[Task[List[Seq[String]]]] = allResources <<= (baseDirectory) map { base =>
    val codeDir = (base / "resources" / "code" / "src")
    val filter = new SimpleFilter((name: String) => name.startsWith("Example"))
    val allFiles = codeDir ** filter
    allFiles.get.map(f => Seq(f.toString)).toList // need to take into account multiple files
  }
  

  
  def scalaRun = TaskKey[sbt.ScalaRun]("sbt-run-inst", "")
  def scalaRunTask = scalaRun <<= myRunnerInit(ExtraTasks)
  
  def quickTestResources = TaskKey[Unit]("quick-run-resources", "Run type debugger on resources (without interaction)")  
  def testResourcesTask = quickTestResources in ExtraTasks <<= (allResources, fullClasspath in ExtraTasks, scalaRun) map { (files, cp, r) =>
    val logger = ConsoleLogger()
    files.foreach { fs => // calling .par will deteriorate the output
      logger.info("Running on : " + fs)
      r.run("scala.typedebugger.TypeDebuggerUI", sbt.Build.data(cp), fs ++ Seq("-Ydetached"), logger)
    }
  } 
  
  lazy val ExtraTasks = config("extra") extend(Compile)
  
  def singleRun = InputKey[Unit]("run-type-debugger", "Run type debugger on file(s)")  
  def singleRunTask = myFullRunInputTask(singleRun, ExtraTasks, "scala.typedebugger.TypeDebuggerUI")

  lazy val root =
    Project("TypeDebugger",file (".")).configs(ExtraTasks).settings((tdsettings ++ Seq(testResourcesTask, scalaRunTask, allResourcesTask, singleRunTask, allResourcesTask, localScala, jars, forkExtra)):_*)
}