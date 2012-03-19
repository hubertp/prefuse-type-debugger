package scala.typedebugger
package ui



import prefuse.{Constants, Display, Visualization}
import prefuse.data.{Graph, Table, Node, Tuple, Edge, Tree}
import prefuse.data.tuple.{TupleSet, DefaultTupleSet}
import prefuse.data.expression.{AbstractPredicate, Predicate, OrPredicate}
import prefuse.visual.expression.{InGroupPredicate, VisiblePredicate}
import prefuse.action.{ ItemAction, ActionList, RepaintAction, Action}
import prefuse.action.assignment.{ColorAction, FontAction}
import prefuse.action.animate.{ColorAnimator, LocationAnimator, QualityControlAnimator, VisibilityAnimator}
import prefuse.action.layout.graph.{NodeLinkTreeLayout}
import prefuse.action.layout.CollapsedSubtreeLayout
import prefuse.activity.SlowInSlowOutPacer
import prefuse.visual.{VisualItem, NodeItem, EdgeItem}
import prefuse.util.{ColorLib, FontLib, GraphicsLib}
import prefuse.controls.{ControlAdapter, FocusControl, PanControl, WheelZoomControl,
                         ZoomControl, ZoomToFitControl}
import prefuse.visual.sort.TreeDepthItemSorter
import prefuse.util.PrefuseLib


import java.awt.Color
import java.awt.event.{ActionEvent, MouseEvent}
import java.awt.geom.{Point2D, Rectangle2D}
import javax.swing.{Action => swingAction, _}
import javax.swing.event.TreeModelListener

import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, Stack, HashMap }


//import javax.swing.{Action => swingAction, _}
import prefuse.render._



object PrefuseComponent {
  val tree = "tree"
  val treeNodes = "tree.nodes"
  val treeEdges = "tree.edges"

  val orientation = Constants.ORIENT_BOTTOM_TOP

  val fixedNodes = "tree.fixed"           // Nodes that are 'fixed' to be visible
  val openGoalNodes = "tree.openGoals"
  val nonGoalNodes = "tree.openNods"      // Intermediate nodes on the path to the goal nodes
  val toRemoveNodes = "tree.removeNodes"  // Nodes to be removed on the refresh of UI
  val linkGroupNodes = "tree.link"
  val clickedNode = "tree.clicked"
  
  val typerNodes = "tree.typer"
  val namerNodes = "tree.namer"
  
  val backgroundColor = Color.WHITE
  val foregroundColor = Color.BLACK
}

import UIConfig.{nodesLabel => label} // todo: remove

// TODO possible to get rid of that?
trait ToExpandInfo {
  def isGoalOrSibling(t: VisualItem): (Boolean, Boolean)
}

trait PrefuseControl {
  def nodeColorAction(nodes: String): ItemAction
  def customTreeLayout(orientation: Int): NodeLinkTreeLayout
  def showFullTree: Boolean
  def extractPrefuseNode(t: Tuple): Node
  def isGoal(t: Tuple): Boolean
  def isNode(t: Tuple): Boolean
  def eventInfo(item: VisualItem): String
  def setGoalPath(item: VisualItem): Unit
  
  def visualizeFixedNodesAction(): Action
  def initialGoalPredicate(): ToExpandInfo
}

abstract class PrefuseComponent(t: Tree) extends Display(new Visualization()) with ui.PrefuseTooltips with PrefuseControl {
  import PrefuseComponent._
  import PrefusePimping._
    
          
  setBackground(backgroundColor)
  setForeground(foregroundColor)
 
  m_vis.add(tree, t)

    // Set default node/edge renderer and orientation
  val m_nodeRenderer = new LabelRenderer(label)
  m_nodeRenderer.setRenderType(AbstractShapeRenderer.RENDER_TYPE_FILL)
  m_nodeRenderer.setHorizontalAlignment(Constants.CENTER)
  m_nodeRenderer.setVerticalAlignment(Constants.CENTER)
  m_nodeRenderer.setVerticalPadding(10)
  m_nodeRenderer.setHorizontalPadding(20)
  m_nodeRenderer.setRoundedCorner(8,8)
  val m_edgeRenderer = new EdgeRenderer(Constants.EDGE_TYPE_LINE)
      
  val rf = new DefaultRendererFactory(m_nodeRenderer)
  rf.add(new InGroupPredicate(treeEdges), m_edgeRenderer)
  m_vis.setRendererFactory(rf)
  
  var m_orientation = orientation

  // colors
  val nodeColor:ItemAction = nodeColorAction(treeNodes)
  val textColor:ItemAction = new ColorAction(treeNodes,
              VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0))
  m_vis.putAction("textColor", textColor)
      
  val edgeColorAction = new ColorAction(treeEdges,
              VisualItem.STROKECOLOR, ColorLib.rgb(194, 194, 194))
  
  edgeColorAction.add(IsEdgeOnGoalPath,
    new ColorAction(treeEdges, VisualItem.STROKECOLOR, ColorLib.rgb(0,0,0)))
  val edgeColor:ItemAction = edgeColorAction

  // quick repaint
  val repaint0 = new ActionList()
  repaint0.add(nodeColor)
  repaint0.add(new RepaintAction())
  m_vis.putAction("repaint", repaint0)
      
  // full paint
  val fullPaint = new ActionList()
  fullPaint.add(nodeColor)
  m_vis.putAction("fullPaint", fullPaint)
      
  // animate paint change
  val animatePaint = new ActionList(400)
  animatePaint.add(new ColorAnimator(treeNodes))
  animatePaint.add(new RepaintAction())
  m_vis.putAction("animatePaint", animatePaint)

  // create the tree layout action
  val treeLayout: NodeLinkTreeLayout = customTreeLayout(m_orientation)
  treeLayout.setLayoutAnchor(new Point2D.Double(25,300))
  m_vis.putAction("treeLayout", treeLayout)
      
  // Animation that handles collapsing/opening nodes 
  // Need to adapt it so that not all non-goal nodes are expanded
  val subLayout = new CollapsedSubtreeLayout(tree, m_orientation)
  m_vis.putAction("subLayout", subLayout)

  val autoPan = new AutoPanAction()

  // create the filtering and layout
  val filter = new ActionList()
  // Includes degree-of-interest factor
  filter.add(visualizeFixedNodesAction())
  filter.add(new UnfocusOnItems)
  filter.add(new VisualizeNodesWithPred(new IsNodeOnGoalPath(openGoalNodes, nonGoalNodes)))
  filter.add(new VisualizeNodes(linkGroupNodes))

  filter.add(new ShowAllGoalsAndEdges(clickedNode))

  filter.add(new FontAction(treeNodes, FontLib.getFont("Tahoma", 16)))
  filter.add(treeLayout)
  filter.add(textColor)
  filter.add(nodeColor)
  filter.add(edgeColor)
  m_vis.putAction("filter", filter)
      
  // animated transition
  val animate = new ActionList(1000)
  animate.setPacingFunction(new SlowInSlowOutPacer())
  animate.add(autoPan)
  animate.add(new QualityControlAnimator())
  animate.add(new VisibilityAnimator(tree))
  animate.add(new LocationAnimator(treeNodes))
  animate.add(new ColorAnimator(treeNodes))
  animate.add(new RepaintAction())
  m_vis.putAction("animate", animate)
  m_vis.alwaysRunAfter("filter", "animate")
      
  // create animator for orientation changes
  val orient = new ActionList(2000)
  orient.setPacingFunction(new SlowInSlowOutPacer())
  orient.add(autoPan)
  orient.add(new QualityControlAnimator())
  orient.add(new LocationAnimator(treeNodes))
  orient.add(new RepaintAction())
  m_vis.putAction("orient", orient)

  // We cache collapse tree because it has a minimal version of
  // the graph that contains all the errors and intermediate nodes leading to it.
  val initialNodes = new CollapseTree(openGoalNodes, clickedNode)
  m_vis.putAction("initial-goals", initialNodes)
 
  val zoomToFit = new ZoomToFitControl()
  val hoverController = new HoverTooltip()
  zoomToFit.setZoomOverItem(false)
  
  // initialize the display
  setSize(700,800)
  setItemSorter(new TreeDepthItemSorter())
  addControlListener(zoomToFit)
  addControlListener(new ZoomControl())
  addControlListener(new WheelZoomControl())
  addControlListener(new PanControl())
  addControlListener(hoverController)
  addControlListener(new FocusControl(1, "filter"))

  setOrientation(orientation)
  m_vis.addFocusGroup(fixedNodes, new DefaultTupleSet())
  m_vis.addFocusGroup(openGoalNodes, new DefaultTupleSet())
  m_vis.addFocusGroup(nonGoalNodes, new DefaultTupleSet())
  m_vis.addFocusGroup(toRemoveNodes, new DefaultTupleSet())
  m_vis.addFocusGroup(linkGroupNodes, new DefaultTupleSet())
  m_vis.addFocusGroup(clickedNode, new DefaultTupleSet())

  // To have the whole tree expanded initially
  // comment out initial-goals
  if (!showFullTree)
    m_vis.run("initial-goals")
  m_vis.run("filter")

  private def setOrientation(orientation0: Int) {
    val rtl = m_vis.getAction("treeLayout").asInstanceOf[NodeLinkTreeLayout]
    val stl = m_vis.getAction("subLayout").asInstanceOf[CollapsedSubtreeLayout]
    orientation0 match {
      case Constants.ORIENT_LEFT_RIGHT =>
          m_nodeRenderer.setHorizontalAlignment(Constants.LEFT)
          m_edgeRenderer.setHorizontalAlignment1(Constants.RIGHT)
          m_edgeRenderer.setHorizontalAlignment2(Constants.LEFT)
          m_edgeRenderer.setVerticalAlignment1(Constants.CENTER)
          m_edgeRenderer.setVerticalAlignment2(Constants.CENTER)
      case Constants.ORIENT_RIGHT_LEFT =>
          m_nodeRenderer.setHorizontalAlignment(Constants.RIGHT)
          m_edgeRenderer.setHorizontalAlignment1(Constants.LEFT)
          m_edgeRenderer.setHorizontalAlignment2(Constants.RIGHT)
          m_edgeRenderer.setVerticalAlignment1(Constants.CENTER)
          m_edgeRenderer.setVerticalAlignment2(Constants.CENTER)
      case Constants.ORIENT_TOP_BOTTOM =>
          m_nodeRenderer.setHorizontalAlignment(Constants.CENTER)
          m_edgeRenderer.setHorizontalAlignment1(Constants.CENTER)
          m_edgeRenderer.setHorizontalAlignment2(Constants.CENTER)
          m_edgeRenderer.setVerticalAlignment1(Constants.BOTTOM)
          m_edgeRenderer.setVerticalAlignment2(Constants.TOP)
      case Constants.ORIENT_BOTTOM_TOP =>
          m_nodeRenderer.setHorizontalAlignment(Constants.CENTER)
          m_edgeRenderer.setHorizontalAlignment1(Constants.CENTER)
          m_edgeRenderer.setHorizontalAlignment2(Constants.CENTER)
          m_edgeRenderer.setVerticalAlignment1(Constants.TOP)
          m_edgeRenderer.setVerticalAlignment2(Constants.BOTTOM)
      case _ =>
          throw new IllegalArgumentException(
              "Unrecognized orientation value: "+orientation)
    }
    m_orientation = orientation0
    rtl.setOrientation(orientation0)
    stl.setOrientation(orientation0)
  }



  // Some predefined actions (direct translation from the prefuse examples)
  class OrientAction(var orientation: Int) extends AbstractAction {
    def actionPerformed(evt: ActionEvent) {
      setOrientation(orientation)
      getVisualization().cancel("orient")
      getVisualization().run("treeLayout")
      getVisualization().run("orient")
    }
  }
  
  class AutoPanAction extends Action {
    private val m_start:Point2D = new Point2D.Double()
    private val m_end:Point2D   = new Point2D.Double()
    private val m_cur:Point2D   = new Point2D.Double()
    private var m_bias: Int  = 100
      
    def run(frac: Double) {
      val ts:TupleSet = m_vis.getFocusGroup(clickedNode)
      if ( ts.getTupleCount() == 0 )
        return
          
      if ( frac == 0.0 ) {
        var xbias = 0
        var ybias = 0
        m_orientation match {
          case Constants.ORIENT_LEFT_RIGHT =>
            xbias = m_bias

          case Constants.ORIENT_RIGHT_LEFT =>
            xbias = -m_bias

          case Constants.ORIENT_TOP_BOTTOM =>
            ybias = m_bias

          case Constants.ORIENT_BOTTOM_TOP =>
            ybias = -m_bias
        }

        val vi:VisualItem = ts.next()
        m_cur.setLocation(getWidth()/2, getHeight()/2)
        getAbsoluteCoordinate(m_cur, m_start)
        m_end.setLocation(vi.getX()+xbias, vi.getY()+ybias)
      } else {
        m_cur.setLocation(m_start.getX() + frac*(m_end.getX()-m_start.getX()),
                         m_start.getY() + frac*(m_end.getY()-m_start.getY()))
        panToAbs(m_cur)
      }
    }
  }

  class HoverTooltip extends ControlAdapter {
    var activeTooltip: PrefuseTooltip = _
  
    override def itemExited(item: VisualItem, e: MouseEvent) {
      if(activeTooltip != null) activeTooltip.stopShowing()
    }
    
    override def itemPressed(item: VisualItem, e: MouseEvent) {
      clearTooltip()
    }
    
    override def itemReleased(item: VisualItem, e: MouseEvent) {
      if(item.isInstanceOf[NodeItem] && e.getButton() == MouseEvent.BUTTON3)
        showNodeTooltip(item, e.getX(), e.getY())
    }
    
    def clearTooltip(): Unit = if(activeTooltip != null) activeTooltip.stopShowingImmediately()
    
    def showItemTooltip(item: VisualItem) {
      //Use fixed coordinates as item can randomly fail to give sensible values
      showNodeTooltip(item, 5, 5)
    }
    
    protected def showNodeTooltip(item: VisualItem, coordX: Int, coordY: Int) {
      val v = item.getVisualization()
      val info = eventInfo(item)
      
      showTooltip(new NodeTooltip("Some name", eventInfo(item), 100, 100, v.getDisplay(0)),
                  item, coordX, coordY)
    }
    
    private def showTooltip(ptt: PrefuseTooltip, item: VisualItem, coordX: Int, coordY: Int) {
      clearTooltip()
      
      activeTooltip = ptt
      activeTooltip.startShowing(coordX + 10, coordY + 5,
          (getWidth()/2) < coordX,
          (getHeight()/2) < coordY)
    }
  }

  class EdgeColorAction(group: String) extends ColorAction(group, VisualItem.STROKECOLOR) {
    // TODO: events should have appropriate colors
    override def getColor(item: VisualItem): Int = {
      ColorLib.rgba(200,200,200,0)
    }
  }
  
  // Add all NodeItems/EdgeItems for which predicate resolves true 
  // to the visible section of the graph
  class VisualizeNodesWithPred(predicate: Predicate) extends Action{
    def run(frac: Double) {
      val ts = m_vis.getFocusGroup(Visualization.FOCUS_ITEMS)
      if (ts != null)
        for (item <- m_vis.items_[Tuple](predicate)) {
          ts.addTuple(item)
        }
    }
  }

  
  // Remove all nodes (and outgoing/incoming edges) that are in the toRemoveGoals group
  // todo: this should be an object but there is a bug in the compiler
  // which I don't have time to track down
  class UnfocusOnItems extends Action {
    //val vis = m_vis // to avoid illegalaccesserror i=during runtime (bug in compiler?)
   
    // todo: again, this should be an object
    class ToRemovePred extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = {
        // because we added nodeItem to the list, not visualItem which 't' is
        val ts = m_vis.getFocusGroup(toRemoveNodes)
        ts != null && isNode(t) && ts.containsTuple(extractPrefuseNode(t))
      }
    }
    
    lazy val pred = new ToRemovePred

    def run(frac: Double) {
      val ts = m_vis.getFocusGroup(Visualization.FOCUS_ITEMS)
      if (ts != null) {
        for (item <- m_vis.items_[NodeItem](pred)) {
          PrefuseLib.updateVisible(item, false)
          item.setExpanded(false)
          item.childEdges_[VisualItem]().foreach(PrefuseLib.updateVisible(_, false))
          item.outNeighbors_[VisualItem]().foreach(PrefuseLib.updateVisible(_, false))
          ts.removeTuple(item)
        }
      }
      m_vis.getFocusGroup(toRemoveNodes).clear()
    }
  }
  

  
  // Need to show all the goals, all edges between them, 
  // as well as immediate (1-distance) subgoals of each goal
  class ShowAllGoalsAndEdges(clickedNode: String) extends Action {
    def run(frac: Double) {
      val ts = m_vis.getFocusGroup(Visualization.FOCUS_ITEMS)
      
      if (ts.getTupleCount() == 0) {
        // in case of no visible nodes available
        // display only the synthetic root
        // add it to clickable nodes (for zooming purposes etc).
        val root = treeLayout.getLayoutRoot()
        ts.addTuple(root)
        val clickedTs = m_vis.getFocusGroup(clickedNode)
        clickedTs.addTuple(root)
      }
      
      for (item <- ts.tuples()) {
        item match {
          case item: NodeItem =>
            PrefuseLib.updateVisible(item, true)
            item.setExpanded(true)
            item.childEdges_[VisualItem]().foreach(PrefuseLib.updateVisible(_, true))
            // neighbors should be added to separate group
            item.outNeighbors_[VisualItem]().foreach(PrefuseLib.updateVisible(_, true))
            
            // If this is not a goal, then expand all the incoming edges as well
            if (!isGoal(item))
              item.inEdges_[VisualItem]().foreach(PrefuseLib.updateVisible(_, true))
          case vItem: VisualItem =>
            PrefuseLib.updateVisible(vItem, true)
          case _ =>
        }
      }
    }
  }
  
  // Collapse the whole tree initially so that only (hard) errors are visible
  class CollapseTree(openGoalsGroup: String, clickedNode: String)
    extends Action {
    
    lazy val minimumVisibleNodes = findLeastCommonSpanningTree(allInitialGoals)
    lazy val predicate           = initialGoalPredicate()
    
    private def findLeastCommonSpanningTree(nodes: List[Node]): List[Node] = {
      //assert(nodes.length > 1)
      if (nodes.length == 1)
        nodes
      else {
	      assert(nodes.length > 1)
	      val idx: HashMap[Node, Int] = HashMap.empty
	      var marked: List[Node] = Nil
	      
	      // with the first node go up to the root
	      var start: Node = nodes.head
	      while (start != null) {
	        idx += start -> 1
	        marked = start :: marked 
	        start = start.getParent()
	      }
	      
	      nodes.tail.foreach( node => { 
	        var start0 = node
	        while (!idx.contains(start0)) {
	          idx += start0 -> 1
	          start0 = start0.getParent()
	        }
	        assert(idx.contains(start0))
	        idx(start0) = idx(start0) + 1
	      })
	      
	      // go backwards from initial list until counter > 1
	      val notmarked = marked.takeWhile(node => idx(node) == 1)
	      val spanningTreeNodes = idx -- notmarked
	      spanningTreeNodes.keys.toList
      }
    }
    
    private def allInitialGoals: List[Node] =  m_vis.getFocusGroup(openGoalsGroup).toList
    
    def run(frac: Double) {
      val items = m_vis.items_[VisualItem](Visualization.ALL_ITEMS)
      val ts = m_vis.getFocusGroup(openGoalsGroup)
      for (item <- items) {
        val (visibleGoal, sibling) = predicate.isGoalOrSibling(item)
        if (!visibleGoal) {
          if (sibling) {
            // add to goals group
            ts.addTuple(extractPrefuseNode(item))
          } else {
            item match {
              case item0: NodeItem =>
                item0.setExpanded(false)
              case _ =>
            }
          }
          PrefuseLib.updateVisible(item, sibling)
        } else {
          // Goal
          val panTs = m_vis.getFocusGroup(clickedNode)
          panTs.addTuple(item)
          if (isNode(item)) {
            setGoalPath(item)
            ts.addTuple(extractPrefuseNode(item))
          }
        }
      }
      
      // If there is more than one error we need to find
      // the least common node between all initial goals
      if (ts.getTupleCount() > 1)
        minimumVisibleNodes.foreach(ts.addTuple)
    }
  }
  
  // ------------------------
  // Some utility predicates
  // ------------------------
      
  // Predicate returning true for goal and nongoal groups
  class IsNodeOnGoalPath(openGoalsGroup: String, nonGoalsGroup: String) extends AbstractPredicate {
    override def getBoolean(t: Tuple): Boolean = {
      isNode(t) && {
        val ts = m_vis.getFocusGroup(openGoalsGroup)
        // because we added nodeItem to the list, not visualItem which 't' is
        val nodeItem = extractPrefuseNode(t)
        (ts != null && ts.containsTuple(nodeItem)) || {
          // fallback, try non goal group
          val ts2 = m_vis.getFocusGroup(nonGoalsGroup)
          ts2 != null && ts2.containsTuple(nodeItem)
        }
      }
    }
  }
  
  // Predicate for checking if an edge is between the two goals
  object IsEdgeOnGoalPath extends AbstractPredicate {
    override def getBoolean(t: Tuple): Boolean = t match {
      case edge: EdgeItem if isNode(edge.getSourceNode) =>
        isGoal(edge.getTargetNode) && isGoal(edge.getSourceNode)
      case _ => false
    }
  }
}