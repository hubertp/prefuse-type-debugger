import sbt._
import Keys._

object TypeDebugger extends Build {
  val scalaSnapshotSuffix = "v2.11.0-instrumented5"
  val prefuseSnapshotSuffix = "0.22-snapshot"
  val typeDebuggerSnapshotSuffix = "v0.4"
  val resourcesBinDir = "resources/lib"
  val snapshotsUrl = "http://lampwww.epfl.ch/~plocinic/typedebugger-snapshots"

  
  val tdsettings = Defaults.defaultSettings ++ Seq(
    organization := "EPFL",
    name         := "type-debugger",
    version      := "0.4",
    scalaVersion := "2.11.0-SNAPSHOT", 
    scalacOptions in Compile ++= Seq("-unchecked"),
    javacOptions +=  "-Xss2M",
    resolvers ++= Seq("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")
    //logLevel     := Level.Debug
  )
  
  lazy val localScala = scalaHome <<= baseDirectory { f =>
    val props = new java.util.Properties()
    IO.load(props, f / "local.properties")
    val x = props.getProperty("scala.instrumented.home")
    if (x == null) {
      // try to use the resources directory
      println("Using resources directory to locate necessary scala jars")
      Some(file("resources"))
      //sys.error("I need scala compiler version with instrumentation on. Define scala.instrumented.home in local.properties")
    } else {
      println("Using: " + x)
      Some(file(x))
    }
  }
  
  lazy val jars = unmanagedJars in Compile <++= (scalaHome, baseDirectory) map { (sHome, base) =>
    val scalaCompiler = (sHome.get / "lib" / "scala-compiler.jar")
    val scalaReflect = (sHome.get / "lib" / "scala-reflect.jar")
    val prefuselib = file(resourcesBinDir + "/prefuse-core-latest.jar")
    val allJars = scalaCompiler +++ scalaReflect +++ prefuselib
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
  
  def pullLatestScalaBin = TaskKey[Unit]("pull-latest-scala-binary", "Gets instrumented version of latest scala (compiler, library, reflection)")
  def pullLatestScalaBinTask: Setting[Task[Unit]] = pullLatestScalaBin <<= (baseDirectory) map { base =>
    // Use scripts in tools to download jars to resources/lib
    val lampUrl = snapshotsUrl + "/com/github/hubertp"
    val scalaPackPrefix = "org/scala-lang/scala-"
    val packages = List("compiler", "library", "reflect")
    packages.foreach { p =>
      val from = lampUrl + "/" + scalaPackPrefix + p + "/" + "scala-" + p + "-" + scalaSnapshotSuffix + ".jar"
      val to = resourcesBinDir + "/scala-" + p + "-latest.jar"
      url(from) #> file(to) !
    }
  }
  
  def pullLatestPrefuseBin = TaskKey[Unit]("pull-latest-prefuse-binary", "Gets snapshot of Prefuse binary")
  def pullLatestPrefuseBinTask: Setting[Task[Unit]] = pullLatestPrefuseBin <<= (baseDirectory) map { base =>
    // Use scripts in tools to download jars to resources/lib
    val lampUrl = snapshotsUrl + "/com/github/hubertp"
    val from = lampUrl + "/prefuse/prefuse-core-" + prefuseSnapshotSuffix + ".jar"
    val to = resourcesBinDir + "/prefuse-core-latest.jar"
    url(from) #> file(to) !
  }
  
  def pullLatestDebuggerBin = TaskKey[Unit]("pull-latest-type-debugger-binary", "Gets latest snapshot of type debugger binary")
  def pullLatestDebuggerBinTask: Setting[Task[Unit]] = pullLatestDebuggerBin <<= (baseDirectory) map { base =>
    // Use scripts in tools to download jars to resources/lib
    val lampUrl = snapshotsUrl + "/com/github/hubertp"
    val from = lampUrl + "/scala/typedebugger/type-debugger-" + typeDebuggerSnapshotSuffix + ".jar"
    val to = resourcesBinDir + "/type-debugger-latest.jar"
    url(from) #> file(to) !
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
    Project("TypeDebugger",file (".")).configs(ExtraTasks).settings((tdsettings ++ Seq(pullLatestScalaBinTask, pullLatestPrefuseBinTask, pullLatestDebuggerBinTask, testResourcesTask, scalaRunTask, allResourcesTask, singleRunTask, allResourcesTask, localScala, jars, forkExtra)):_*)
}