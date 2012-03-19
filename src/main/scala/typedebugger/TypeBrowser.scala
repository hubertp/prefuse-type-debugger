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
                           with processing.PrefusePostProcessors
                           with processing.StringOps
                           with ui.controllers.PrefuseControllers
                           with ui.controllers.SwingControllers
                           with ui.UIUtils{
  import global.EV
  import EV._
  
  import UIConfig.{nodesLabel => label}

  def buildStructure(srcs: List[String], settings: TypeDebuggerSettings, fxn: Filter, nodesLabel: String) = {
    val builder = new CompilerRunWithEvents(srcs, nodesLabel)
    //TODO include settings
    builder.run(fxn)
    // provide prefuse-specific structure
    val prefuseTree = new Tree()
    val (root, initial) = EventNodeProcessor.processTree(prefuseTree, builder.root,
                                                       builder.initialGoals, nodesLabel)

    if (settings.debugTD.value)
      println("[errors] " + initial.map(_.ev))

    if (settings.fullTypechecking.value) (prefuseTree, Nil) else (prefuseTree, initial)
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

    val (prefuseStructure, goals) = buildStructure(srcs, settings, filtr, label)
    val prefuseController = new PrefuseController(prefuseStructure, goals)
    val basicSwingFrame = new SwingFrame(prefuseController, "Type debugger 0.0.3", srcs)
    val swingController = new TypeDebuggerController(basicSwingFrame)
    //val frame = new TypeDebuggerFrame(prefuseTree, srcs, goals)
    val lock = new Lock()
    swingController.frame.createFrame(lock)
    //frame.createFrame(lock)
    lock.acquire
  }
}
