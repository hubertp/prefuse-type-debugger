package scala.typedebugger

import java.awt.{List => awtList, _}
import java.awt.geom.{Point2D, Rectangle2D}
import java.awt.event._

import java.io.File

import scala.concurrent.Lock
import scala.collection.mutable
import scala.tools.nsc.io

import prefuse.data.Tree
import prefuse.Visualization
import ui.{UIConfig, SwingFrame}

// combine all parts into a single module
abstract class TypeBrowser extends AnyRef
                           with internal.CompilerInfo
                           with internal.PrefuseStructure
                           with internal.StructureBuilders
                           with internal.CompilationTools
                           with internal.EventFiltering
                           with processing.PrefusePostProcessors
                           with processing.StringOps
                           with processing.Hooks
                           with ui.controllers.PrefuseControllers
                           with ui.controllers.SwingControllers
                           with ui.UIUtils {
  import global.{EV, NoPosition}
  import EV._
  
  import UIConfig.{nodesLabel => label}
  
  private var builder: CompilerWithEventInfo = _
    
  private val prefuseTrees = new mutable.HashMap[io.AbstractFile, Tree]()

  def compileFullAndProcess(srcs: List[io.AbstractFile], settings: TypeDebuggerSettings, fxn: Filter) = {
    builder = new CompilerRunWithEvents(label, fxn)
    //TODO include settings
    val result = builder.run(srcs)
    postProcess(result)
  }

  
  def targetedCompile(pos: global.Position, hook: PostCompilationHook) = {
    debug("Targeted compile: " + pos)
    assert(builder != null, "need full compiler run first") // todo: remove restriction
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
    val initial = EventNodeProcessor.processTree(prefuseTrees.toMap, res.root,
                                                 res.goals, label)
    debug("[errors] " + initial.map(_.ev))

    if (settings.fullTypechecking.value) Nil else initial
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

  def compileAndShow(srcs: List[String], settings: TypeDebuggerSettings) {
    val filtr =  Filter.and(Filter pf {
      // TODO shouldn't filter out accidentally the events 
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
      case _: ErrorEvent                  => true
      case _: ContextTypeError            => true
      case _: LubEvent                    => true
      case _: TypesEvent                  => true
      case _: RecoveryEvent               => true // TODO need to remove that dependency
                                                  // but then it brakes our indentation mechanism
                                                  // indentation needs to be separated from filtering stuff
                                                  // ATM opening/closing events cannot be filtered out at this point
      case _: NamerApplyPhase             => false
      case uDone: UnitApplyDone if uDone.phase.name == "namer" => false // should rather expose the current
                                                                        // in global and use namerPhase?
      case _: CompilationUnitEvent        => true
    }, EVDSL.ph <= 4)

    //assert(srcs.length == 1, "[debugger limitation] restrict debugging to one file")
    val sources = realSources(srcs)
    // init data trees
    sources foreach (src => prefuseTrees(src) = new Tree())
    
    val goals = compileFullAndProcess(sources, settings, filtr)
    
    val swingController = new SwingController(sources)
    swingController.initAllDisplays(prefuseTrees, goals)
    val lock = new Lock()
    swingController.createFrame(lock)
    //frame.createFrame(lock)
    lock.acquire
  }
}
