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
import scala.collection.mutable
import scala.tools.nsc.io

import prefuse.render._



object PrefuseComponent {
  val tree = "tree"                       // data structure that stored the whole debugging tree

  val typeDebuggerorientation = Constants.ORIENT_BOTTOM_TOP

  val stickyNodes = "tree.sticky"         // Nodes that are 'fixed' to be visible
  val openGoalNodes = "tree.openGoals"
  val nonGoalNodes = "tree.openNods"      // Intermediate nodes on the path to the goal nodes
  val toRemoveNodes = "tree.removeNodes"  // Nodes to be removed on the refresh of UI
  //val linkGroupNodes = "tree.link"
  val clickedNode = "tree.clicked"
  val visibleGroup = "tree.visible"
  
  val backgroundColor = Color.WHITE
  val foregroundColor = Color.BLACK
  
  def toVisualNode(node: Node, vis: Visualization): NodeItem = vis.getVisualItem(tree, node).asInstanceOf[NodeItem]
  def toVisualEdge(edge: Edge, vis: Visualization): EdgeItem = vis.getVisualItem(tree, edge).asInstanceOf[EdgeItem]
}

import UIConfig.{nodesLabel => label} // todo: remove

trait AdvancedOptions {
  def enableOption(v: Filtering.Value): Unit
  def disableOption(v: Filtering.Value): Unit
  def isOptionEnabled(t: Tuple): Boolean
  def isAdvancedOption(t: Tuple): Boolean  
}

abstract class PrefuseComponent(source: io.AbstractFile, t: Tree) extends Display(new Visualization()) with ui.PrefuseTooltips {
  import PrefuseComponent._
  import PrefusePimping._
  
  protected def nodeColorAction(nodes: String): ItemAction
  protected def showFullTree: Boolean
  protected def extractPrefuseNode(t: Tuple): Node
  protected def isNode(t: Tuple): Boolean
  protected def eventInfo(item: VisualItem): util.StringFormatter
  
  private var edgeRenderer: EdgeRenderer = _
  private var nodeRenderer: LabelRenderer = _
  protected var orientation: Int = _
  private var treeLayout: NodeLinkTreeLayout = new CustomNodeLinkTreeLayout(orientation, 50, 0, 8)
  protected def debug(msg: => String): Unit
  
  // TODO: reduce visibility?
  def treeRoot(): NodeItem = treeLayout.getLayoutRoot()
  
  
  protected val lst = new CachedLeastSpanningTree()
  def nodesAlwaysVisible: List[NodeItem] = {
    lst.get()
  }
  
  def getBottomNode: NodeItem = {
    lst.getRoot()
  }
  
  private val hoverController = new HoverTooltip() 
  def tooltipController = hoverController
  
  def adv: AdvancedOptions
  private def isAdvEnabled(t: Tuple): Boolean = adv.isAdvancedOption(t) && adv.isOptionEnabled(t)
  
  protected def _goals(): List[NodeItem]

  def init () {
    setBackground(backgroundColor)
    setForeground(foregroundColor)
   
    m_vis.add(tree, t)
    
    val treeNodes = "tree.nodes"
    val treeEdges = "tree.edges"
  
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
    //treeLayout =  customTreeLayout()
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
    filter.add(new VisualizeStickyNodes())
    filter.add(new UnfocusOnItems)
    filter.add(new VisualizeNodesWithPred(new IsNodeOnGoalPath()))
    //filter.add(new VisualizeNodes(linkGroupNodes))
  
    filter.add(new ShowAllGoalsAndEdges())
  
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
    m_vis.putAction("initial-goals", new CollapseTree())
   
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
    m_vis.addFocusGroup(stickyNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(openGoalNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(nonGoalNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(toRemoveNodes, new DefaultTupleSet())
    //m_vis.addFocusGroup(linkGroupNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(clickedNode, new DefaultTupleSet())
    m_vis.addFocusGroup(visibleGroup, new DefaultTupleSet())
    //m_vis.getFocusGroup(Visualization.FOCUS_ITEMS).addTupleSetListener(new SearchFor(826))
    
  
    m_vis.putAction("advancedOptions", new CollapseDisabled())
    showPrefuseDisplay()
  }
  
  def reRenderDisabledEvents() {
    m_vis.run("advancedOptions")
  }
  
  def reRenderView() {
    debug("re-render view")
    m_vis.run("filter")
  }
  
  protected def showPrefuseDisplay() {
    debug("generate full prefuse display")
    if (!showFullTree)
	    m_vis.run("initial-goals")
	  m_vis.run("filter")
  }
  
  class SearchFor(id: Int) extends prefuse.data.event.TupleSetListener {
    def tupleSetChanged(ts: prefuse.data.tuple.TupleSet, added: Array[Tuple], removed: Array[Tuple]) {
      added.toList foreach { t =>
        if (t.getRow == id)
          println("Added the searched-for tuple " + t)
      }
    }
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
    lst.clear()
    m_vis.getFocusGroup(stickyNodes).clear()
    m_vis.getFocusGroup(openGoalNodes).clear()
    m_vis.getFocusGroup(nonGoalNodes).clear()
    m_vis.getFocusGroup(toRemoveNodes).clear()
    //m_vis.getFocusGroup(linkGroupNodes).clear()
    m_vis.getFocusGroup(clickedNode).clear()
    m_vis.getFocusGroup(visibleGroup).clear()
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
      
      showTooltip(new NodeTooltip(eventInfo(item), 100, 100, v.getDisplay(0)),
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
  class VisualizeNodesWithPred(predicate: Predicate) extends Action {
    def run(frac: Double) {
      val ts = m_vis.getFocusGroup(visibleGroup)
      if (ts != null)
        for (item <- m_vis.items_[Tuple](predicate)) {
          ts.addTuple(item)
        }
    }
  }

  // cannot be an object, because of a bug in the compiler (#2296)
  // illegal runtime access on m_vis
  class UnfocusOnItems extends Action {
    def run(frac: Double) {
      val ts = m_vis.getFocusGroup(visibleGroup)
      val visItems = m_vis.items_[Node](toRemoveNodes).map(toVisualNode(_, m_vis))
      for (item <- visItems) {
        PrefuseLib.updateVisible(item, false)
        item.setExpanded(false)
        item.childEdges_[VisualItem]().foreach(PrefuseLib.updateVisible(_, false))
        item.outNeighbors_[VisualItem]().foreach(PrefuseLib.updateVisible(_, false))
        if (item.getParentEdge() != null)
          PrefuseLib.updateVisible(toVisualEdge(item.getParentEdge(), m_vis), false)
        ts.removeTuple(item)
      }
      m_vis.getFocusGroup(toRemoveNodes).clear()
    }
  }
  
  // Need to show all the goals, all edges between them, 
  // as well as immediate (1-distance) subgoals of each goal
  class ShowAllGoalsAndEdges() extends Action {
    
    def run(frac: Double) {
      val ts = m_vis.getFocusGroup(visibleGroup)
      
      if (ts.getTupleCount() == 0) {
        // in case of no visible nodes available
        // display only the synthetic root
        // add it to clickable nodes (for zooming purposes etc).
        val root = treeLayout.getLayoutRoot()
        ts.addTuple(root)
        val clickedTs = m_vis.getFocusGroup(clickedNode)
        clickedTs.addTuple(root)
      }
      
      debug("[make visible] " + ts.tuples().toList)
      for (item <- ts.tuples()) {
        item match {
          case item: NodeItem =>
            PrefuseLib.updateVisible(item, true)
            item.setExpanded(true)
            item.childEdges_[EdgeItem]().foreach { edge =>
              val targetNode = edge.getTargetNode()
              if (!edge.isVisible && (!adv.isAdvancedOption(targetNode) || adv.isOptionEnabled(targetNode)))
                PrefuseLib.updateVisible(edge, true)
            }
            // neighbors should be added to separate group
            item.outNeighbors_[NodeItem]().foreach { neighbor =>
              if (!neighbor.isVisible && (!adv.isAdvancedOption(neighbor) || adv.isOptionEnabled(neighbor)))
                PrefuseLib.updateVisible(neighbor, true)
            }
            
            // If this is not a goal, then expand all the incoming edges as well
            val ts = m_vis.getFocusGroup(openGoalNodes)
            if (!(ts containsTuple item.getSourceTuple))
              item.inEdges_[VisualItem]().foreach(PrefuseLib.updateVisible(_, true))
          case vItem: EdgeItem =>
            PrefuseLib.updateVisible(vItem, true)
          case _ =>
        }
      }
    }
  }
  
  // Spanning tree that wraps aroound goals/errors
  class CachedLeastSpanningTree() {
    private[this] var cachedMinimumSet: Option[(List[NodeItem], NodeItem)] = None
    private[this] var cachedPathToTreeRoot: List[NodeItem] = Nil
    @inline def ensureInitialized() {
      cachedMinimumSet match {
        case Some(_) =>
        case None =>
          // cache was flushed/not_initialized, calculate from scratch
          cachedMinimumSet = Some(leastSpanningTree(_goals))
          cachedPathToTreeRoot = pathToTreeRoot(cachedMinimumSet.get._2)
      }
    }
    
    def get(): List[NodeItem] = {
      ensureInitialized()
      cachedMinimumSet.get._1
    }
    
    def getRoot(): NodeItem = {
      ensureInitialized()
      cachedMinimumSet.get._2
    }
    
    def getPathToRoot(): List[NodeItem] = {
      ensureInitialized()
      cachedPathToTreeRoot
    }
    
    def clear() {
      cachedMinimumSet = None
    }
    
    private def pathToTreeRoot(start: NodeItem): List[NodeItem] = {
      var buffer = new mutable.ListBuffer[NodeItem]()
      var node = start
      
      while (node != null) {
        buffer += node
        node = node.getParent.asInstanceOf[NodeItem]
      }
      buffer.toList
    }
    
    private def leastSpanningTree(nodes: List[NodeItem]): (List[NodeItem], NodeItem) = {
      if (nodes.length == 1)
        (nodes, nodes.head)
      else {
        assert(nodes.length > 1)
        val idx = mutable.HashMap.empty[Node, Int]
        var meetingPoint: Option[Node] = None
        var maybeRoot: Node = null
        
        
        def markPath(start: Node) {
          var visited = new mutable.ListBuffer[Node]()
          var current = start
          // 1) we reached root
          // 2) we reached node that was already on some root path
          // 3) we reached advanced node that is not enabled
          while (current != null && !idx.contains(current) && (!adv.isAdvancedOption(current) || adv.isOptionEnabled(current))) {
            visited += current
            current = current.getParent()
          }
          
          if (current != null) {
            if (idx.contains(current)) {
              // 2)
              visited foreach (v => idx += (v -> 1))
              idx(current) += 1
              meetingPoint match {
                case Some(n) =>
                  if (idx(n) < idx(current))
                    meetingPoint = Some(current)
                case None    =>
                  meetingPoint = Some(current)
              }
              
              // increase the counter for all the
              // nodes above the join until we reach root
              current = current.getParent()
              while (current != null) {
                idx(current) += 1
                current = current.getParent()
              }
            } else {
              // 3)
            }
          } else {
            // 1)
            maybeRoot = visited.last
            visited foreach (v => idx += (v -> 1))
          }
        }
        nodes foreach markPath
        
        def cleanupUpwards(root: Node) {
          var start = root.getParent
          while (start != null) {
            idx -= start
            start = start.getParent
          }
        }
        val root = meetingPoint match {
          case Some(n) =>
            cleanupUpwards(n)
            n
          case None    =>
            // some paths were invalidated
            maybeRoot
        }
        
        val res = idx.keys.toList map(toVisualNode(_, m_vis))
        assert(root != null)
        (res, toVisualNode(root, m_vis)) 
      }
    }
  }
  
  class CustomNodeLinkTreeLayout(orientation: Int, dspace: Double, bspace: Double, tspace: Double)
    extends NodeLinkTreeLayout(tree) {//, orientation, dspace, bspace, tspace) {
    
    // Anchor the layout root at the first error
    // or show the synthetic root
    // whenever we expand the type tree we update the root
    override def getLayoutRoot() = {      
      var item:Node = lst.getRoot()
      if (item == null) {
        super.getLayoutRoot()
      } else {
        while (item.getParent() != null && toVisualNode(item.getParent, m_vis).isVisible)
          item = item.getParent()
        toVisualNode(item, m_vis)
      }
    }
    
    override def getGraph(): Graph = {
      m_vis.getGroup(visibleGroup).asInstanceOf[Graph]
    }
  }
  
  
  // Collapse the whole tree initially so that only goals are visible (and paths to them)
  class CollapseTree() extends Action {    
    private def allInitialGoals: TupleSet = m_vis.getFocusGroup(openGoalNodes)

    def run(frac: Double) {
      val visibleItems = m_vis.items_[VisualItem](Visualization.ALL_ITEMS)
      val ts = allInitialGoals
      val min = lst.get()
      for (item <- visibleItems) {
        if (min contains item) {
          // Goal
          val panTs = m_vis.getFocusGroup(clickedNode)
          panTs.addTuple(item)
          if (isNode(item)) {
            ts.addTuple(item.getSourceTuple)
          }
        } else {
          item match {
            case item0: NodeItem =>
              item0.setExpanded(false)
            case _ =>
          }
          PrefuseLib.updateVisible(item, false)
        }
      }
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
        if (node.isVisible && adv.isAdvancedOption(node) && !adv.isOptionEnabled(node)) {
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
  class IsNodeOnGoalPath() extends AbstractPredicate {
    override def getBoolean(t: Tuple): Boolean = t match {
      case item: NodeItem =>
        val ts = m_vis.getFocusGroup(openGoalNodes)
        ts.containsTuple(item.getSourceTuple) || {
          val ts2 = m_vis.getFocusGroup(nonGoalNodes)
          ts2.containsTuple(item.getSourceTuple)
        }
      case _ =>
        false
    }
  }
  
  // Predicate for checking if an edge is between the two goals
  object IsEdgeOnGoalPath extends AbstractPredicate {
    override def getBoolean(t: Tuple): Boolean = t match {
      case edge: EdgeItem if isNode(edge.getSourceNode) =>
        val ts = m_vis.getFocusGroup(openGoalNodes)
        ts.containsTuple(edge.getTargetNode.asInstanceOf[VisualItem].getSourceTuple) &&
        ts.containsTuple(edge.getSourceNode.asInstanceOf[VisualItem].getSourceTuple)
      case _ => false
    }
  }
  
  // Add all intermediate nodes that lead to the already visible nodes
  // to the nonGoalGroup (i.e. not goals, but still visible)
  class VisualizeStickyNodes() extends Action {    
    override def run(frac: Double) {
      val target = m_vis.getFocusGroup(stickyNodes)
      target.foreach(addLinkPath)
    }
    
    def addLinkPath(starting: NodeItem) {
      var n: NodeItem = starting
      val nonGoals = m_vis.getFocusGroup(nonGoalNodes)
      val cachedPath = lst.getPathToRoot()
      debug("[fixed nodes]: " + n)
      while (!(cachedPath contains n) && n.getParent != null) {
        nonGoals.addTuple(n.getSourceTuple)
        n = n.getParent.asInstanceOf[NodeItem]
      }
      // some opengoals might have been removed in the meantime
      // need to ensure that they are there
      val goals = m_vis.getFocusGroup(openGoalNodes)
      if (cachedPath contains n) {
        // traverse from the root of the lst up to n
        // and make sure that they are visible
        var node = lst.getRoot()
        while (node != n) {
          goals.addTuple(node.getSourceTuple)
          node = node.getParent.asInstanceOf[NodeItem]
        }
        goals.addTuple(node.getSourceTuple)
      } else {
        cachedPath foreach (goals.addTuple)
      }
    }
  }
  
  class VisualizeNodes(groupName: String) extends Action {
    def run(frac: Double) {
      val target = m_vis.getFocusGroup(visibleGroup)
      val ts = m_vis.getFocusGroup(groupName)
      ts.tuples().foreach(n => target.addTuple(n.asInstanceOf[Tuple]))
    }
  }

}