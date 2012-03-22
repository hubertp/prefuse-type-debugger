package scala.typedebugger

import java.awt.{List => awtList, _}
import java.awt.geom.{Point2D, Rectangle2D}
import java.awt.event._

import java.io.File
import javax.swing.{Action => swingAction, _}
import javax.swing.event.TreeModelListener

//import javax.swing.text.{Highlighter, DefaultHighlighter}

import scala.concurrent.Lock
import scala.collection.mutable.{ ListBuffer, Stack, HashMap }
import scala.collection.JavaConversions._
import scala.tools.nsc.io
import scala.tools.nsc.util.{SourceFile, BatchSourceFile}

import prefuse.data.{Graph, Table, Node, Tuple, Edge, Tree}
import prefuse.data.tuple.{TupleSet, DefaultTupleSet}
import prefuse.data.io.TreeMLReader
import prefuse.data.expression.{AbstractPredicate, Predicate, OrPredicate}
import prefuse.util.PrefuseLib
import prefuse.{Constants, Display, Visualization}
import prefuse.action._
import prefuse.action.animate.{ColorAnimator, LocationAnimator, QualityControlAnimator, VisibilityAnimator}
import prefuse.action.assignment.{ColorAction, FontAction}
import prefuse.action.filter.{FisheyeTreeFilter, VisibilityFilter}
import prefuse.activity.SlowInSlowOutPacer
import prefuse.controls.{ControlAdapter, FocusControl, PanControl, WheelZoomControl,
                         ZoomControl, ZoomToFitControl}
import prefuse.action.layout.CollapsedSubtreeLayout
import prefuse.action.layout.graph.NodeLinkTreeLayout
import prefuse.visual.{VisualItem, NodeItem, EdgeItem}
import prefuse.visual.expression.{InGroupPredicate, VisiblePredicate}
import prefuse.visual.sort.TreeDepthItemSorter
import prefuse.util.{ColorLib, FontLib, GraphicsLib}
import prefuse.util.display.{DisplayLib}
import prefuse.util.ui.{JFastLabel, JSearchPanel}
import prefuse.render._

import ui.{UIConfig, SwingFrame}

// combine all parts into a single module
abstract class TypeBrowser extends AnyRef
                           with internal.CompilerInfo
                           with internal.PrefuseStructure
                           with internal.StructureBuilders
                           with internal.Tools
                           with processing.PrefusePostProcessors
                           with processing.StringOps
                           with ui.controllers.PrefuseControllers
                           with ui.controllers.SwingControllers
                           with ui.UIUtils{
  import global.{EV, NoPosition}
  import EV._
  
  import UIConfig.{nodesLabel => label}
  
  private var builder: CompilerWithInstrumentation {
    def root: BaseTreeNode[EventNode]
    def initialGoals: List[BaseTreeNode[EventNode]]
  } = _
    
  private val prefuseTree = new Tree()
  private var prefuseController: PrefuseController = _

  def compileFullAndProcess(srcs: List[io.AbstractFile], settings: TypeDebuggerSettings, fxn: Filter) = {
    builder = new CompilerRunWithEvents(label, fxn)
    //TODO include settings
    builder.run(srcs)
    postProcess()
  }

  
  def targetedCompile(pos: global.Position) = {
    println("Targeted compile: " + pos)
    updateTreeAndProcess(pos)
  }
  
  private def updateTreeAndProcess(pos: global.Position) = {
    assert(builder != null, "need full compiler run first")
    val overlappingStat = global.locateStatement(pos)
    val statPos = if (overlappingStat.pos.isRange && !overlappingStat.pos.isTransparent) overlappingStat.pos else NoPosition
    builder.runTargeted(pos, statPos)
    prefuseTree.clear()
    val processedGoals = postProcess()
    prefuseController.updateGoals(processedGoals)
  }

  // provide prefuse-specific structure
  private def postProcess() = {
    val (root, initial) = EventNodeProcessor.processTree(prefuseTree, builder.root,
                                                       builder.initialGoals, label)
    if (settings.debugTD.value)
      println("[errors] " + initial.map(_.ev))

    if (settings.fullTypechecking.value) Nil else initial
  }
  
  private def realSources(srcRoots: List[String]): List[io.AbstractFile] = {
    val srcs = new ListBuffer[String]
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
    }, EVDSL.ph <= 4)

    assert(srcs.length == 1, "[debugger limitation] restrict debugging to one file")
    val sources = realSources(srcs)
    val goals = compileFullAndProcess(sources, settings, filtr)
    prefuseController = new PrefuseController(prefuseTree, goals)
    prefuseController.init() // can move later?
    val basicSwingFrame = new SwingFrame(prefuseController, "Type debugger 0.0.3", sources)
    val swingController = new TypeDebuggerController(basicSwingFrame)
    //val frame = new TypeDebuggerFrame(prefuseTree, srcs, goals)
    val lock = new Lock()
    swingController.frame.createFrame(lock)
    //frame.createFrame(lock)
    lock.acquire
  }
}
