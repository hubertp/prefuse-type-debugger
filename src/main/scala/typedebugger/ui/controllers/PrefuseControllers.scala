package scala.typedebugger
package ui
package controllers

import prefuse.{Constants, Display, Visualization}
import prefuse.data.{Graph, Table, Node, Tuple, Edge, Tree}
import prefuse.data.expression.{AbstractPredicate, Predicate, OrPredicate}
import prefuse.action.{ ItemAction }
import prefuse.action.layout.graph.{NodeLinkTreeLayout}
import prefuse.action.assignment.{ColorAction, FontAction}
import prefuse.action.{ ItemAction, ActionList, RepaintAction, Action}
import prefuse.visual.{VisualItem, NodeItem, EdgeItem}
import prefuse.util.{ColorLib}
import scala.collection.JavaConversions._

import scala.collection.mutable
import scala.tools.nsc.io

trait PrefuseControllers {
  self: internal.CompilerInfo with UIUtils with internal.EventFiltering 
    with internal.PrefuseStructure with PrefuseStringOps =>
    
  import PrefuseDisplay._
  import PrefusePimping._
   
  import global.{Tree => STree, _}
  import EV._
  
  class PrefuseController(idx: Int, source: io.AbstractFile, pTree: Tree, vis: TypeDebuggerVisualization,
    goals0: List[UINode[PrefuseEventNode]], contr: AdvancedOptionsController)
    extends PrefuseDisplay(source, pTree, vis)(idx) {
    
    // methods that are global specific
    def nodeColorAction(nodes: String): ItemAction = new NodeColorAction(nodes, this)
    def extractPrefuseNode(t: Tuple): Node = asDataNode(t).pfuseNode
    def isNode(t: Tuple): Boolean = containsDataNode(t)
    def showFullTree = settings.fullTypechecking.value
    def debug(msg: => String) = self.debug(msg, "ui")
    
    def eventFullInfo(item: VisualItem): util.StringFormatter = fullStringOps(item)
    def eventShortInfo(item: VisualItem): String = shortStringOps(item)

    private[this] var verifiedGoals: List[NodeItem] = null
    private def initGoals(ls: List[UINode[PrefuseEventNode]]): Unit = {
      def verifyUpToRoot(n: UINode[PrefuseEventNode]): Boolean = 
        !n.advanced && (!n.parent.isDefined || verifyUpToRoot(n.parent.get)) 
      val pNodes = ls filter verifyUpToRoot map (node => toVisualNode(node.pfuseNode, m_vis, dataGroupName))
      debug("verified initial goals: " + pNodes)
      verifiedGoals = pNodes
    }
    
    def _goals: List[NodeItem] = {
      if (verifiedGoals == null)
       initGoals(goals0)
      verifiedGoals
    }
    
    def updateGoals(gs: List[UINode[PrefuseEventNode]]) {
      debug("[prefuse] update initial goals to: " + gs)
      initGoals(gs)
      flushVisCache()
    }

    protected def adv: AdvancedOptionsController = contr
  }
  
  object NodeColorAction {
    private final val DEFAULT_COLOR: Int = ColorLib.rgba(255,255,255,0)
    private val phasesMap: Map[Phase, (Int, Int, Int)] = 
      Map((global.currentRun.namerPhase, (192, 255, 193)), (global.currentRun.typerPhase, (238, 230, 133)))
  }

  class NodeColorAction(group: String, display: PrefuseDisplay) extends ColorAction(group, VisualItem.FILLCOLOR) {
    import NodeColorAction._
    private def retrieveEvent(item: VisualItem): Option[Event] =
      if (containsDataNode(item)) Some(asDataNode(item).ev)
      else None

    // TODO: Refactor that
    override def getColor(item: VisualItem): Int = {
      val event = retrieveEvent(item)
      event match {
        case _ if ( m_vis.isInGroup(item, display.clickedNodes)) =>
          ColorLib.rgb(198, 229, 250) // Make it always visible
        case Some(ev: HardErrorEvent) =>
          ColorLib.rgba(255, 0, 0, 150)
        case Some(ev: ContextTypeError) if ev.errType == ErrorLevel.Hard =>
          ColorLib.rgba(255, 0, 0, 150)
        case Some(ev: TyperOmittedStatement) =>
          ColorLib.rgba(204, 204, 204, 50)
        case Some(ev: SoftErrorEvent) =>
          ColorLib.rgba(255, 0, 0, 50)
        case Some(ev: ContextTypeError) if ev.errType == ErrorLevel.Soft =>
          ColorLib.rgba(255, 0, 0, 50)
        case Some(ev: LubEvent) =>
          ColorLib.rgba(238, 102, 34, 100)
        case Some(ev: TypesEvent) =>
          ColorLib.rgba(238, 102, 34, 100) // TODO change to a different one
        case _ =>
          // search currently not supported
          if ( m_vis.isInGroup(item, Visualization.SEARCH_ITEMS) )
            ColorLib.rgb(255,190,190)
          else if ( m_vis.isInGroup(item, display.stickyNodes) )
            ColorLib.rgb(204, 255, 51)
          else if ( m_vis.isInGroup(item, Visualization.FOCUS_ITEMS) )
            ColorLib.rgb(198,229,229)
          else if ( m_vis.isInGroup(item, display.visibleGroup) )
            ColorLib.rgb(198,229,229)
            // provide different colors for phases and types of events
          else if ( item.getDOI() > -1 )
            ColorLib.rgb(164,193,193)
          else
            event match {
              case Some(ev: AdaptEvent) =>
                ColorLib.rgba(150, 200, 100, 100)
              case Some(ev: InferEvent) =>
                ColorLib.rgba(201, 150, 100, 100)
              case Some(ev) =>
                if (phasesMap.contains(ev.phase)) {
                  val c = phasesMap(ev.phase)
                  ColorLib.rgb(c._1, c._2, c._3)
                } else
                  DEFAULT_COLOR                   
              case None =>
                DEFAULT_COLOR
            }
      }
    }
      
  }
}