package scala.typedebugger

import java.awt.{List => awtList, _}
import java.awt.event._

import java.io.File

import scala.concurrent.Lock
import scala.collection.mutable
import scala.tools.nsc.io

import prefuse.data.Tree

import internal.DebuggerSettings

// combine all parts into a single module
abstract class TypeBrowser extends AnyRef
   with internal.CompilerInfo
   with internal.PrefuseStructure
   with internal.StructureBuilders
   with internal.CompilationTools
   with internal.EventFiltering
   with internal.SyntheticEvents
   with processing.PrefusePostProcessors
   with processing.Hooks
   with stringops.StringOps
   with ui.controllers.PrefuseControllers
   with ui.controllers.SwingControllers
   with ui.controllers.PrefuseStringOps
   with ui.UIUtils {
  import global.{EV, NoPosition}
  import EV._
  
  import ui.UIConfig.{nodesLabel => label}
  
  private var builder: CompilerRunWithEventInfo = _
  private val prefuseTrees = new mutable.HashMap[io.AbstractFile, Tree]()

  def compileFullAndProcess(srcs: List[io.AbstractFile], settings: DebuggerSettings, fxn: Filter) = {
    builder = new InstrumentedRun(label, fxn)
    //TODO include settings
    val result = builder.run(srcs)
    postProcess(result)
  }
  
  def targetedCompile(pos: global.Position, hook: PostCompilationHook) = {
    debug("Targeted compile: " + pos)
    assert(builder != null, "need full compiler run first")
    val overlappingTree = global.locate(pos) //global.locateStatement(pos)
    val treePos = if (overlappingTree.pos.isRange && !overlappingTree.pos.isTransparent) overlappingTree.pos else NoPosition
    val result = builder.runTargeted(pos, treePos)
    val tree = prefuseTrees(global.positionToFile(pos))
    tree.clear()
    EventDescriptors.clearCache()
    val processedGoals = postProcess(result)
    hook.info(processedGoals)
  }

  // provide prefuse-specific structure
  private def postProcess(res: CompilerRunResult) = {
    val goals = EventNodeProcessor.processTree(prefuseTrees.toMap, res.root,
                                                 res.goals, label)
    debug("[goals-in-focus] " + goals.map(_.ev))
    if (settings.fullTypechecking.value) Nil else goals
  }
  
  private def realSources(srcRoots: List[String]): List[io.AbstractFile] = {
    val srcs = new mutable.ListBuffer[String]
    srcRoots foreach { p =>
      io.Path(p).walk foreach { x =>
        if (x.isFile && x.hasExtension("scala", "java"))
          srcs += x.path
      }
    }
    srcs.toList.distinct.map(s => io.AbstractFile.getFile(s))
  }

  def compileAndShow(srcs: List[String], settings: DebuggerSettings) {
    val filtr =  Filter.and(Filter pf {
      // shouldn't filter out accidentally the events 
      // that open/close blocks -> this can cause unexpected graphs
      case _: TyperTypeSet                => false
      case _: DebugEvent                  => false
      case _: TyperEvent                  => true
      case _: ImplicitMethodTpeAdaptEvent => true
      case _: InferEvent                  => true
      case _: ImplicitEvent               => true
      case _: AdaptToEvent                => true
      case _: DoTypedApplyEvent           => true
      case _: NamerEvent                  => true
//        case _: ValidateParentClassEvent    => true
      //case _: TyperDone => true
      case _: AdaptEvent                  => true
      case _: TypingBlockEvent            => true
//        case _: NewContext                  => true
      case _: ErrorEventInfo              => true
      case _: LubEvent                    => true
      case _: TypesEvent                  => true
      case _: RecoveryEvent               => true // indentation needs to be separated from filtering logic
                                                  // ATM opening/closing events cannot be filtered out at this point
      case _: NamerApplyPhase             => false
      case uDone: UnitApplyDone if uDone.phase.name == "namer" => false // should rather expose the current
                                                                        // in global and use namerPhase?
      case _: CompilationUnitEvent        => true
    }, EVDSL.ph <= 4)

    val sources = realSources(srcs)
    sources foreach (src => prefuseTrees(src) = new Tree())
    
    val goals = compileFullAndProcess(sources, settings, filtr)
    
    val swingController = new SwingController(sources)
    swingController.initAllDisplays(prefuseTrees, goals)
    
    val lock = new Lock()
    swingController.createFrame(lock, settings.detached.value)
    if (settings.detached.value)
      new Thread("Start and close type debugger") {
        override def run() {
          Thread.sleep(100)
          swingController.jframe.fireCloseEvent() 
        }
      }.run()
    lock.acquire
  }
}
