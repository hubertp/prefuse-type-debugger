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
import scala.collection.mutable.HashMap

import prefuse.render._



object PrefuseComponent {
  val tree = "tree"
  val treeNodes = "tree.nodes"
  val treeEdges = "tree.edges"

  val typeDebuggerorientation = Constants.ORIENT_BOTTOM_TOP

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

abstract class PrefuseComponent(t: Tree) extends Display(new Visualization()) with ui.PrefuseTooltips {
  import PrefuseComponent._
  import PrefusePimping._
  
  protected def nodeColorAction(nodes: String): ItemAction
  protected def showFullTree: Boolean
  protected def extractPrefuseNode(t: Tuple): Node
  protected def isGoal(t: Tuple): Boolean
  protected def isNode(t: Tuple): Boolean
  protected def eventInfo(item: VisualItem): String
  protected def setGoalPath(item: VisualItem): Unit
  protected def visualizeFixedNodesAction(): Action
  
  protected def customTreeLayout(): NodeLinkTreeLayout
  protected def initialGoalPredicate(): ToExpandInfo
  
  private var edgeRenderer: EdgeRenderer = _
  private var nodeRenderer: LabelRenderer = _
  protected var orientation: Int = _
  private var treeLayout: NodeLinkTreeLayout = _
  
  def treeRoot(): Node = if (treeLayout != null) treeLayout.getLayoutRoot() else null
  
  private val collapsedTreeAction =  new CollapseTree(openGoalNodes, clickedNode)
  def nodesAlwaysVisible: List[Node] = {
    collapsedTreeAction.minimumVisibleNodes
  }
  private val hoverController = new HoverTooltip() 
  def tooltipController = hoverController
  
  def enableOption(v: Filtering.Value): Unit
  def disableOption(v: Filtering.Value): Unit
  protected def isOptionEnabled(t: Tuple): Boolean
  protected def isAdvancedOption(t: Tuple): Boolean
  

  def init () {
    setBackground(backgroundColor)
    setForeground(foregroundColor)
   
    m_vis.add(tree, t)
  
      // Set default node/edge renderer and orientation
    nodeRenderer = new LabelRenderer(label)
    nodeRenderer.setRenderType(AbstractShapeRenderer.RENDER_TYPE_FILL)
    nodeRenderer.setHorizontalAlignment(Constants.CENTER)
    nodeRenderer.setVerticalAlignment(Constants.CENTER)
    nodeRenderer.setVerticalPadding(10)
    nodeRenderer.setHorizontalPadding(20)
    nodeRenderer.setRoundedCorner(8,8)
    edgeRenderer = new EdgeRenderer(Constants.EDGE_TYPE_LINE)
        
    val rf = new DefaultRendererFactory(nodeRenderer)
    rf.add(new InGroupPredicate(treeEdges), edgeRenderer)
    m_vis.setRendererFactory(rf)
    
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
    treeLayout =  customTreeLayout()
    treeLayout.setLayoutAnchor(new Point2D.Double(25,300))
    m_vis.putAction("treeLayout", treeLayout)
        
    // Animation that handles collapsing/opening nodes 
    // Need to adapt it so that not all non-goal nodes are expanded
    val subLayout = new CollapsedSubtreeLayout(tree, orientation)
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
    m_vis.putAction("initial-goals", collapsedTreeAction)
   
    val zoomToFit = new ZoomToFitControl()
    zoomToFit.setZoomOverItem(false)
    
    // initialize the display
    setSize(700,800)
    setItemSorter(new TreeDepthItemSorter())
    addControlListener(zoomToFit)
    addControlListener(new ZoomControl())
    addControlListener(new WheelZoomControl())
    addControlListener(new PanControl())
    addControlListener(tooltipController)
    addControlListener(new FocusControl(1, "filter"))
  
    setOrientation(typeDebuggerorientation)
    m_vis.addFocusGroup(fixedNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(openGoalNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(nonGoalNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(toRemoveNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(linkGroupNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(clickedNode, new DefaultTupleSet())
  
    m_vis.putAction("advancedOptions", new CollapseDisabled())
    showPrefuseDisplay()
  }
  
  def reRenderDisabledEvents() {
    m_vis.run("advancedOptions")
  }
  
  def reRenderProof() {
    m_vis.run("filter")
  }
  
  protected def showPrefuseDisplay() {
    if (!showFullTree)
	    m_vis.run("initial-goals")
	  m_vis.run("filter")
  }

  private def setOrientation(orientation0: Int) {
    val rtl = m_vis.getAction("treeLayout").asInstanceOf[NodeLinkTreeLayout]
    val stl = m_vis.getAction("subLayout").asInstanceOf[CollapsedSubtreeLayout]
    orientation0 match {
      case Constants.ORIENT_LEFT_RIGHT =>
        nodeRenderer.setHorizontalAlignment(Constants.LEFT)
        edgeRenderer.setHorizontalAlignment1(Constants.RIGHT)
        edgeRenderer.setHorizontalAlignment2(Constants.LEFT)
        edgeRenderer.setVerticalAlignment1(Constants.CENTER)
        edgeRenderer.setVerticalAlignment2(Constants.CENTER)
      case Constants.ORIENT_RIGHT_LEFT =>
        nodeRenderer.setHorizontalAlignment(Constants.RIGHT)
        edgeRenderer.setHorizontalAlignment1(Constants.LEFT)
        edgeRenderer.setHorizontalAlignment2(Constants.RIGHT)
        edgeRenderer.setVerticalAlignment1(Constants.CENTER)
        edgeRenderer.setVerticalAlignment2(Constants.CENTER)
      case Constants.ORIENT_TOP_BOTTOM =>
        nodeRenderer.setHorizontalAlignment(Constants.CENTER)
        edgeRenderer.setHorizontalAlignment1(Constants.CENTER)
        edgeRenderer.setHorizontalAlignment2(Constants.CENTER)
        edgeRenderer.setVerticalAlignment1(Constants.BOTTOM)
        edgeRenderer.setVerticalAlignment2(Constants.TOP)
      case Constants.ORIENT_BOTTOM_TOP =>
        nodeRenderer.setHorizontalAlignment(Constants.CENTER)
        edgeRenderer.setHorizontalAlignment1(Constants.CENTER)
        edgeRenderer.setHorizontalAlignment2(Constants.CENTER)
        edgeRenderer.setVerticalAlignment1(Constants.TOP)
        edgeRenderer.setVerticalAlignment2(Constants.BOTTOM)
      case _ =>
        throw new IllegalArgumentException(
            "Unrecognized orientation value: "+orientation0)
    }
    orientation = orientation0
    rtl.setOrientation(orientation0)
    stl.setOrientation(orientation0)
  }
  
  protected def flushVisCache() {
    collapsedTreeAction.cachedMinimumSet = None
    m_vis.getFocusGroup(fixedNodes).clear()
    m_vis.getFocusGroup(openGoalNodes).clear()
    m_vis.getFocusGroup(nonGoalNodes).clear()
    m_vis.getFocusGroup(toRemoveNodes).clear()
    m_vis.getFocusGroup(linkGroupNodes).clear()
    m_vis.getFocusGroup(clickedNode).clear()
    m_vis.getFocusGroup(Visualization.FOCUS_ITEMS).clear()
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
        orientation match {
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
      if(item.isInstanceOf[NodeItem] && e.getButton == MouseEvent.BUTTON3)
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

  class UnfocusOnItems extends Action { 
    object ToRemovePred extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = {
        // because we added nodeItem to the list, not visualItem which 't' is
        val ts = m_vis.getFocusGroup(toRemoveNodes)
        ts != null && isNode(t) && ts.containsTuple(extractPrefuseNode(t))
      }
    }

    def run(frac: Double) {
      val ts = m_vis.getFocusGroup(Visualization.FOCUS_ITEMS)
      if (ts != null) {
        for (item <- m_vis.items_[NodeItem](ToRemovePred)) {
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
            item.childEdges_[EdgeItem]().foreach { edge =>
              val targetNode = edge.getTargetNode()
              if (!edge.isVisible && (!isAdvancedOption(targetNode) || isOptionEnabled(targetNode)))
                PrefuseLib.updateVisible(edge, true)
            }
            // neighbors should be added to separate group
            item.outNeighbors_[NodeItem]().foreach { neighbor =>
              if (!neighbor.isVisible && (!isAdvancedOption(neighbor) || isOptionEnabled(neighbor)))
                PrefuseLib.updateVisible(neighbor, true)
            }
            
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
    
    private[PrefuseComponent] var cachedMinimumSet: Option[List[Node]] = None
    
    def minimumVisibleNodes: List[Node] = cachedMinimumSet match {
      case Some(v) => v
      case None =>
        // cache was flushed, calculate from scratch
        val all = allInitialGoals.toList
        cachedMinimumSet = Some(findLeastCommonSpanningTree(all))
        cachedMinimumSet.get
    }
    
    private def allInitialGoals: TupleSet = m_vis.getFocusGroup(openGoalsGroup)

    private def findLeastCommonSpanningTree(nodes: List[Node]): List[Node] = {
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
    
    def run(frac: Double) {
      val items = m_vis.items_[VisualItem](Visualization.ALL_ITEMS)
      val ts = allInitialGoals
      val predicate = initialGoalPredicate()
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
  
  class CollapseDisabled() extends Action {
    // todo similar to visualitemsearchpred
    object OnlyNodes extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = t match {
        case item: NodeItem if isNode(t) => true
        case _                           => false
      }
    }
    def run(frac: Double) {
      val nodes = m_vis.items_[NodeItem](Visualization.ALL_ITEMS, OnlyNodes)
      for (node <- nodes) {
        if (node.isVisible && isAdvancedOption(node) && !isOptionEnabled(node)) {
          PrefuseLib.updateVisible(node, false)
          node.setExpanded(false)
          node.inEdges_[EdgeItem]() foreach { edge =>
            PrefuseLib.updateVisible(edge, false)
          }
        }
      }
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