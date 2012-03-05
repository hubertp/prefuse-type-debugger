package scala.typedebugger

import java.awt.{List => awtList, _}
import java.awt.geom.{Point2D, Rectangle2D}
import java.awt.event._

import java.io.File
import javax.swing.{Action => swingAction, _}
import javax.swing.event.TreeModelListener

import javax.swing.text.{Highlighter, DefaultHighlighter}

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

import scala.tools.nsc.io.{File => ScalaFile}

abstract class TypeBrowser extends AnyRef
                           with internal.CompilerInfo
                           with internal.IStructure
                           with internal.PrefuseStructure
                           with internal.StructureBuilders
                           with processing.PrefusePostProcessors
                           with processing.StringOps {
 
  import global.{Tree => STree, _}
  import EV._
  
  type UINodeP = UINode[PrefuseEventNode]
  

  object TypeDebuggerFrame {
    val BACKGROUND = Color.WHITE
    val FOREGROUND = Color.BLACK
    val COLUMN_PREFUSENODE_CLASS = (new PrefuseEventNode(null, null, null)).getClass
  }

  object TreeDisplay {
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
      
    // Search for visual item corresponding to node (i.e. Node ==> NodeItem)
    class VisualItemSearchPred(search: Node, label: String) extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = {
        if (t.canGet(label, TypeDebuggerFrame.COLUMN_PREFUSENODE_CLASS) && t.isInstanceOf[NodeItem]) {
          // because we added nodeItem to the list, not visualItem which 't' is
          val nodeItem = t.get(label).asInstanceOf[UINodeP]
          nodeItem.pfuseNode == search
        } else false
      }
    } 
  }

  class TreeDisplay(t: Tree, label: String, initialGoals: List[UINodeP])
    extends Display(new Visualization()) with ui.PrefuseTooltips {
    
    treeViewSelf => 
    
    import TreeDisplay._
    import TypeDebuggerFrame._
    
    m_vis.add(tree, t)

    // Set default renderer and orientation
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
    val nodeColor:ItemAction = new NodeColorAction(treeNodes)
    val textColor:ItemAction = new ColorAction(treeNodes,
                VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0))
    m_vis.putAction("textColor", textColor)
        
    val edgeColorAction = new ColorAction(treeEdges,
                VisualItem.STROKECOLOR, ColorLib.rgb(194, 194, 194))
//    val edgeColor: ItemAction = new EdgeColorAction(treeEdges)
    edgeColorAction.add(new MainGoalPathEdgePredicate(openGoalNodes),
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
    val treeLayout = new CustomNodeLinkTreeLayout(tree, Visualization.FOCUS_ITEMS,
      m_orientation, 50, 0, 8)
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
    filter.add(new VisualizeFixedNodes(fixedNodes, nonGoalNodes))
    filter.add(new UnfocusOnItems(Visualization.FOCUS_ITEMS, toRemoveNodes))
    filter.add(new VisualizeNodesWithPred(Visualization.FOCUS_ITEMS,
                                new GoalPathPredicate(openGoalNodes, nonGoalNodes)))
    filter.add(new VisualizeNodes(Visualization.FOCUS_ITEMS, linkGroupNodes))

    filter.add(new ShowAllGoalsAndEdges(Visualization.FOCUS_ITEMS,
                                        clickedNode))

    filter.add(new FontAction(treeNodes, FontLib.getFont("Tahoma", 16)))
    filter.add(treeLayout)
    //filter.add(subLayout)
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
    // TODO refactor
    val initialNodes = new CollapseTree(Visualization.ALL_ITEMS,
                                        openGoalNodes,
                                        clickedNode)
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
    if (!settings.fullTypechecking.value)
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

    class CustomLabelRenderer(label0: String) extends LabelRenderer(label0) {
      override protected def getText(item: VisualItem): String = {
        if (item.canGet(label0, COLUMN_PREFUSENODE_CLASS)) {
          val eNode = item.get(label0).asInstanceOf[UINodeP]
          eNode.ev.eventString
        } else null
      }
    }
    
    class CustomNodeLinkTreeLayout(wholeTree: String, visGroup: String,
      orientation: Int, dspace: Double, bspace: Double, tspace: Double)
      extends NodeLinkTreeLayout(wholeTree) {
      
      object GoalNode extends AbstractPredicate {
        override def getBoolean(t: Tuple): Boolean = {
          if (t.canGet(label, COLUMN_PREFUSENODE_CLASS) && t.isInstanceOf[NodeItem]) {
            // because we added nodeItem to the list, not visualItem which 't' is
            val nodeItem = t.get(label).asInstanceOf[UINodeP]
            nodeItem.goal && t.asInstanceOf[NodeItem].isVisible
          } else false
        }
      }
      
      // Anchor the layout root at the first error
      // or show the synthetic root
      // TODO expand to more errors
      override def getLayoutRoot(): NodeItem = {
        val allVisibleGoals = m_vis.items(visGroup, GoalNode)
        val allPNodeVisibleGoals = allVisibleGoals.map(t => {
          val t0 = t.asInstanceOf[NodeItem]
          (t0.get(label).asInstanceOf[UINodeP].pfuseNode, t0)
        }).toMap
        
        initialGoals match {
          case head::_ =>
            // Need to find respective VisualItem for node so that
            // we can match prefuse node stored in PrefuseEventNode
            var eNode = head 
            while (eNode.parent.isDefined && allPNodeVisibleGoals.contains(eNode.parent.get.pfuseNode)) {
              eNode = eNode.parent.get
            }
            if (!allPNodeVisibleGoals.contains(eNode.pfuseNode)) {
              // we are dealing with a first (root) node
              // so try to find it manually
              val first = m_vis.items(wholeTree, new VisualItemSearchPred(head.pfuseNode, label))
              if (first.hasNext) first.next.asInstanceOf[NodeItem] else super.getLayoutRoot()
            } else {
              allPNodeVisibleGoals(eNode.pfuseNode) // get corresponding visualitem
            }
          case _ =>
            super.getLayoutRoot()
        }
      }
      
      override def getGraph(): Graph = {
        m_vis.getGroup(visGroup).asInstanceOf[Graph]
      }
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
        // Visualization.FOCUS_ITEMS doesn't work reliably here
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

          val vi = ts.tuples().next().asInstanceOf[VisualItem]
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
        
    // TOOLTIPS    
    class HoverTooltip extends ControlAdapter {
      var activeTooltip: PrefuseTooltip = _
    
      override def itemExited(item: VisualItem, e: MouseEvent) {
        if(activeTooltip != null)
          activeTooltip.stopShowing()
      }
      
      override def itemPressed(item: VisualItem, e: MouseEvent) {
        clearTooltip()
      }
      
      override def itemReleased(item: VisualItem, e: MouseEvent) {
        if(item.isInstanceOf[NodeItem] && e.getButton() == MouseEvent.BUTTON3)
          showNodeTooltip(item, e.getX(), e.getY())
      }
      
      def showItemTooltip(item: VisualItem) {
        //Use fixed coordinates as item can randomly fail to give sensible values
        showNodeTooltip(item, 5, 5)
      }
      
      def clearTooltip() {
        if(activeTooltip != null)
          activeTooltip.stopShowingImmediately()
      }
      
      protected def showNodeTooltip(item: VisualItem, coordX: Int, coordY: Int) {
        val v = item.getVisualization()
        val eNode = item.get(label).asInstanceOf[UINodeP]
        
        showTooltip(new NodeTooltip("Some name",
          eNode.fullInfo,
          100, 100, v.getDisplay(0)),
            item, coordX, coordY)
      }
      
      private def showTooltip(ptt: PrefuseTooltip, item: VisualItem, coordX: Int, coordY: Int) {
        if(activeTooltip != null) {
          activeTooltip.stopShowingImmediately()
        }
        
        activeTooltip = ptt
        activeTooltip.startShowing(coordX + 10, coordY + 5,
            (getWidth()/2) < coordX,
            (getHeight()/2) < coordY)
      }
    }
        
    object NodeColorAction {
      private final val DEFAULT_COLOR: Int = ColorLib.rgba(255,255,255,0)
      private val phasesMap: Map[Phase, (Int, Int, Int)] = 
        Map((global.currentRun.namerPhase, (192, 255, 193)), (global.currentRun.typerPhase, (238, 230, 133)))
    }

    class NodeColorAction(group: String) extends ColorAction(group, VisualItem.FILLCOLOR) {
      import NodeColorAction._
      private def retrieveEvent(item: VisualItem): Option[Event] =
        if (item.canGet(label, COLUMN_PREFUSENODE_CLASS))
          Some(item.get(label).asInstanceOf[UINodeP].ev)
        else
          None

      // TODO: Refactor that
      override def getColor(item: VisualItem): Int = {
        val event = retrieveEvent(item)
        event match {
          // TODO
          case _ if ( m_vis.isInGroup(item, TreeDisplay.clickedNode)) =>
            ColorLib.rgb(198, 229, 250) // Make it always visible
          case Some(ev: HardErrorEvent) =>
            ColorLib.rgba(255, 0, 0, 150)
          case Some(ev: ContextTypeError) if ev.errType == ErrorLevel.Hard =>
            ColorLib.rgba(255, 0, 0, 150)
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
            //else if ( m_vis.isInGroup(item, TreeDisplay.clickedNode))
            //  ColorLib.rgb(198, 229, 250)
            else if ( m_vis.isInGroup(item, fixedNodes) )
              ColorLib.rgb(204, 255, 51)
            else if ( m_vis.isInGroup(item, Visualization.FOCUS_ITEMS) )
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
    
    class EdgeColorAction(group: String) extends ColorAction(group, VisualItem.STROKECOLOR) {
      // TODO: events should have appropriate colors
      override def getColor(item: VisualItem): Int = {
        ColorLib.rgba(200,200,200,0)
      }
    }



    // Add all NodeItems/EdgeItems for which predicate resolves true 
    // to the visible section of the graph
    class VisualizeNodesWithPred(visGroup: String, predicate: Predicate) extends Action{
      def run(frac: Double) {
        import scala.collection.JavaConversions._
        val items = m_vis.items(predicate).map(_.asInstanceOf[Tuple])
        val ts = m_vis.getFocusGroup(visGroup)
        if (ts != null) {
          for (item <- items) {
            ts.addTuple(item)
          }
        }
      }
    }
    
    // Add all nodes from groupName to the visible elements of the graph
    // Similar to FocusOnItems but doesn't take a predicate
    class VisualizeNodes(visGroup: String, groupName: String) extends Action {
      def run(frac: Double) {
        val target = m_vis.getFocusGroup(visGroup)
        val ts = m_vis.getFocusGroup(groupName)
        ts.tuples().foreach(n => target.addTuple(n.asInstanceOf[Tuple]))
      }
    }
    
    // Add all intermediate nodes that lead to the already visible nodes
    // to the nonGoalGroup (i.e. not goals, but still visible)
    class VisualizeFixedNodes(fixedGroup: String, nonGoalGroup: String)
      extends VisualizeNodes(fixedGroup, "") {
      override def run(frac: Double) {
        val target = m_vis.getFocusGroup(fixedGroup)
        target.tuples().foreach(n =>
          addLinkPath(n.asInstanceOf[NodeItem]))
      }
      
      def addLinkPath(starting: NodeItem) {
        var n = starting.get(label).asInstanceOf[UINodeP]
        val tsNonGoal = m_vis.getFocusGroup(nonGoalGroup)
        while (!n.goal && n.parent.isDefined) {
          tsNonGoal.addTuple(n.pfuseNode)
          n = n.parent.get
        }
      }
    }
    
    // Remove all nodes (and outgoing/incoming edges) that are in the toRemoveGoals group
    class UnfocusOnItems(visGroup: String, toRemoveGoals: String) extends Action{
     
      class ToRemovePredicate(toRemoveGroup: String) extends AbstractPredicate {
        val ts = m_vis.getFocusGroup(toRemoveGroup)
        
        override def getBoolean(t: Tuple): Boolean = {
          if (ts != null && t.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
            // because we added nodeItem to the list, not visualItem which 't' is
            val nodeItem = t.get(label).asInstanceOf[UINodeP].pfuseNode
            ts.containsTuple(nodeItem)
          } else false
        }
      }

      def run(frac: Double) {
        import scala.collection.JavaConversions._
        val pred = new ToRemovePredicate(toRemoveGoals)
        val items = m_vis.items(pred).map(_.asInstanceOf[Tuple])
        val ts = m_vis.getFocusGroup(visGroup)
        if (ts != null) {
          for (item <- items) {
            val item0 = item.asInstanceOf[NodeItem]
            PrefuseLib.updateVisible(item0, false)
            item0.setExpanded(false)
            item0.childEdges().foreach(edge => PrefuseLib.updateVisible(edge.asInstanceOf[VisualItem], false))
            item0.outNeighbors().foreach(node => PrefuseLib.updateVisible(node.asInstanceOf[VisualItem], false))
            ts.removeTuple(item0)
          }
        }
        val toRemove = m_vis.getFocusGroup(toRemoveGoals)
        toRemove.clear()
      }
    }
    

    
    // Need to show all the goals, all edges between them, 
    // as well as immediate (1-distance) subgoals of each goal
    class ShowAllGoalsAndEdges(visGroup: String, clickedNode: String) extends Action {
      
      def run(frac: Double) {
        import scala.collection.JavaConversions._
        val ts = m_vis.getFocusGroup(visGroup)
        
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
          PrefuseLib.updateVisible(item.asInstanceOf[VisualItem], true)
          item match {
            case item0: NodeItem =>
              item0.setExpanded(true)
              item0.childEdges().foreach(edge =>
                { 
                  PrefuseLib.updateVisible(edge.asInstanceOf[VisualItem], true)
                })
              item0.outNeighbors().foreach(node =>
                {
                  PrefuseLib.updateVisible(node.asInstanceOf[VisualItem], true)
                  // neigbors should be added to separate group
                })
              // If this is not a goal, then expand all the incoming edges as well
              val eNode = item0.get(label).asInstanceOf[UINodeP]
              if (!eNode.goal)
                item0.inEdges().foreach(edge =>
                  PrefuseLib.updateVisible(edge.asInstanceOf[VisualItem], true)
                )
            case _ =>
          }
        }
      }
    }
    
    // Collapse the whole tree initially so that only (hard) errors are visible
    class CollapseTree(group: String, openGoalsGroup: String, clickedNode: String)
      extends Action {
      
      val toExpand = new ListBuffer[NodeItem]()
      
      object InitialGoalPredicate extends AbstractPredicate {
        private def isInitialGoal(node: UINodeP) =
          if (initialGoals.contains(node)) {
            node.ev match {
              case _: HardErrorEvent => true
              case e: ContextTypeError if e.errType == ErrorLevel.Hard => true
              case _ => true
            }
          } else false
        
        override def getBoolean(t: Tuple): Boolean = {
          if (t.isInstanceOf[NodeItem]) {
            val e = t.asInstanceOf[NodeItem]
            if (e.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
              val eNode = e.get(label).asInstanceOf[UINodeP]
  
              // Apart from expanding the error node
              // expand also its siblings
              if (!isInitialGoal(eNode)) {
                if (eNode.children.exists(isInitialGoal))
                  toExpand.add(t.asInstanceOf[NodeItem])
                false
              } else true
            } else false
          } else false
        }
      }
      
      private def setGoalPath(eNode: Option[UINodeP]) {
        eNode match {
          case Some(n) if !n.goal =>
            n.goal = true
            setGoalPath(n.parent)
          case _ =>
        }   
      }
      
      // TODO: better algorithm 
      private def findLeastCommonSpanningTree(nodes: List[Node]): List[Node] = {
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
      
      private def allInitialGoals: List[Node] = {
        m_vis.getFocusGroup(openGoalsGroup).tuples().toList.map(_.asInstanceOf[Node])
      }
      
      lazy val minimumVisibleNodes: List[Node] =
        findLeastCommonSpanningTree(allInitialGoals)

      def run(frac: Double) {
        val items = m_vis.items(group)
        val ts = m_vis.getFocusGroup(openGoalsGroup)
        for (item <- items) {
          val item0 = item.asInstanceOf[VisualItem]
//          if (item0.isInstanceOf[NodeItem])
//            item0.setFont(font)
          val visible = InitialGoalPredicate.getBoolean(item0)
          if (!visible) {
            PrefuseLib.updateVisible(item0, false)
            if (item0.isInstanceOf[NodeItem])
              item0.asInstanceOf[NodeItem].setExpanded(false)
          } else {
            // Goal
            val panTs = m_vis.getFocusGroup(clickedNode)
            //val item0 = item.asInstanceOf[Tuple]
            panTs.addTuple(item0)
            if (item0.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
              val eNode = item0.get(label).asInstanceOf[UINodeP]
              //eNode.goal = true // cache
              // set all parents up to the root with goal path
              setGoalPath(Some(eNode))
              ts.addTuple(eNode.pfuseNode)
            }
          }
        }
        
        // If there is more than one error we need to find
        // the least common node between all initial goals
        if (ts.getTupleCount() > 1) {
          // Find least common ancestor
          //val allgoals:List[Node] = ts.tuples().toList.map(_.asInstanceOf[Node])
          //val spanningTreeNodes = findLeastCommonSpanningTree(allgoals)
          minimumVisibleNodes.foreach(ts.addTuple)
        }
        
        toExpand.foreach(nItem => {
          PrefuseLib.updateVisible(nItem, true)
          // add to goals group
          ts.addTuple(nItem.get(label).asInstanceOf[UINodeP].pfuseNode)
        })
      }
    }
    
    // ------------------------
    // Some utility predicates
    // ------------------------
    // TODO refactor that part?
        
    // Predicate returning true for goal and nongoal groups
    // TODO place in specific action?
    class GoalPathPredicate(openGoalsGroup: String, nonGoalsGroup: String) extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = {
        if (t.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
          val ts = m_vis.getFocusGroup(openGoalsGroup)
          // because we added nodeItem to the list, not visualItem which 't' is
          val nodeItem = t.get(label).asInstanceOf[UINodeP].pfuseNode
          val res0 = ts != null && ts.containsTuple(nodeItem)
          if (res0)
            true
          else {
            // try non goal group
            val ts2 = m_vis.getFocusGroup(nonGoalsGroup)
            ts2 != null && ts2.containsTuple(nodeItem)
          }
        } else false
      }
    }
    
    // Predicate for checking if an edge is between the two goals
    class MainGoalPathEdgePredicate(goalsGroup:String) extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean =
        if (t.isInstanceOf[EdgeItem]) {
          val e = t.asInstanceOf[EdgeItem]
          if (e.getSourceNode.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
            val tNode = e.getTargetNode.get(label).asInstanceOf[UINodeP]
            val sNode = e.getSourceNode.get(label).asInstanceOf[UINodeP]
            tNode.goal && sNode.goal
            //val vis = e.getVisualization()
            //val ts = vis.getFocusGroup(goalsGroup)
            
            //ts.containsTuple(tNode.pfuseNode) && ts.containsTuple(sNode.pfuseNode)
          } else false
        } else false
    }
  }

  class TypeDebuggerFrame(t: Tree, srcs: List[String], label: String, goals: List[UINodeP]) {

    import TypeDebuggerFrame._
    val frame = new JFrame("Type Debugger 0.0.4")
    val topPane = new JPanel(new BorderLayout())

    val treeTransformedViewer = new JTextArea(30, 90)
    val treeGeneralViewer = new JTextArea(30, 30)
    private val treeViewerHighlighter = treeGeneralViewer.getHighlighter()
    private val cleanupNodesAction = new CleanupAction(TreeDisplay.openGoalNodes,
                                                       TreeDisplay.nonGoalNodes,
                                                       TreeDisplay.toRemoveNodes,
                                                       TreeDisplay.linkGroupNodes)
    val treeHighlighter = new HiglighterAndGeneralInfo()
    
    val treeView = new TreeDisplay(t, label, goals)
    var lastClicked: Option[NodeItem] = None

    // Displays all info related to the specific event
    // TODO: we should customize it at this point, by building necessary
    // string representation here in the type debugger rather than
    // redirecting to eventString in the compiler
    // val contextInfoArea = new JTextArea(30, 120) // TODO introduce more interactive widget
    
    private def fullEventInfo(ev: EV.Event) {
      if (settings.debugTD.value && ev != null) {
        println("----------------")
        println("ITEM [" + ev.id + "] CLICKED: " + ev.getClass)
        ev match {
          case e0: TreeEvent => println("TREE POS: " + e0.tree.pos)
          case e0: SymEvent  => println("SYM POS: " + e0.sym.pos)
          case _ =>
        }
        ev match {
          case e0: SymbolReferencesEvent => println("References symbol: " + e0.references.map(_.pos))
          case e0: TreeReferencesEvent   => println("References tree: " + e0.references.map(_.pos))
          case _ =>
        }
        
        if (ev.isInstanceOf[DoneBlock])
          println("DONE BLOCK: " + ev.asInstanceOf[DoneBlock].originEvent)
        if (ev.isInstanceOf[TyperTyped]) {
          val nTyperTyped = ev.asInstanceOf[TyperTyped]
          val expl = nTyperTyped.expl
          println("[TYPER-TYPED] : " + expl + " " + nTyperTyped.tree.getClass + " ||" +
            expl.getClass)
        }  
        println("----------------")
      }
    }

    def createFrame(lock: Lock): Unit = {
      lock.acquire // keep the lock until the user closes the window
      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      frame.addWindowListener(new WindowAdapter() {
        override def windowClosed(e: WindowEvent): Unit = lock.release
      })

      val tabFolder = new JTabbedPane()

      
      treeView.setBackground(BACKGROUND)
      treeView.setForeground(FOREGROUND)
 
      // Split right part even further
      val topSplitPane = 
        new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
          treeView,
          new JScrollPane(tabFolder))
      topSplitPane.setResizeWeight(0.7)
 

      topPane.add(topSplitPane)
      tabFolder.addTab("Tree", null,
        new JScrollPane(treeGeneralViewer))
      tabFolder.addTab("Transformed tree", null,
        new JScrollPane(treeTransformedViewer))


//      contextInfoArea.setFont(new Font("monospaced", Font.PLAIN, 14))
  //    contextInfoArea.setEditable(false)


      // Add listeners
      treeView.addControlListener(new HiglighterAndGeneralInfo())
      
      //treeView.addControlListener(new FocusOnNode(TreeView.fixedNodes))
      treeView.addControlListener(new AddGoal())
      treeView.addControlListener(new LinkNode(TreeDisplay.linkGroupNodes,
                                               TreeDisplay.treeNodes,
                                               TreeDisplay.nonGoalNodes,
                                               TreeDisplay.openGoalNodes))
      treeView.addControlListener(new FixedNode(TreeDisplay.fixedNodes,
                                                TreeDisplay.nonGoalNodes,
                                                TreeDisplay.toRemoveNodes))
      treeView.addControlListener(new KeyPressAddGoal())

      if (srcs.isEmpty)
        println("[Warning] No files specified for debugging.")
      loadFile(srcs.head)
      frame.getContentPane().add(topPane)
      frame.pack()
      frame.setVisible(true)
    }
    
    class HiglighterAndGeneralInfo() extends ControlAdapter {
        override def itemClicked(item: VisualItem, e: MouseEvent) {
          if (item.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
            val node = item.get(label).asInstanceOf[UINodeP]
            if (e.isAltDown())
              fullEventInfo(node.ev)
//            contextInfoArea.setText(node.fullInfo)
          }
        }

        override def itemEntered(item: VisualItem, e: MouseEvent) {

          if (item.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
            clearHighlight()
            val node = item.get(label).asInstanceOf[UINodeP]
            node.ev match {
              case e:TreeEvent if e.tree.pos.isRange =>
                val prettyTree = asString(e.tree)
                treeTransformedViewer.setText(prettyTree)
                highlight(e.tree.pos, TreeMainHighlighter)
                
              case e: SymEvent if e.sym.pos.isRange =>
                highlight(e.sym.pos, TreeMainHighlighter)
                
              case _ =>
                if (settings.debugTD.value)
                  println("No precise position for " + node + ". Trying parents.")
                // Fallback try to find the closest parent that has positions set
                def canHighlight(n: UINodeP) = n.ev match {
                  case e: TreeEvent if e.tree.pos.isRange => true
                  case e: SymEvent if e.sym.pos.isRange => true
                  case _ => false
                }
                
                var nodeWithPos: Option[UINodeP] = node.parent
                while (nodeWithPos.isDefined && !canHighlight(nodeWithPos.get)) {
                  nodeWithPos = nodeWithPos.get.parent
                }
                
                // TODO duplicate code
                if (nodeWithPos.isDefined) {
                  nodeWithPos.get.ev match {
                    case e: TreeEvent => highlight(e.tree.pos, TreeMainHighlighter)
                    case e: SymEvent  => highlight(e.sym.pos, TreeMainHighlighter)
                    case _            => ()
                  }
                }
            }
            
            node.ev match {
              case e: SymbolReferencesEvent =>
                e.references.foreach((ref:Symbol) => if (ref != null && ref != NoSymbol) highlight(ref.pos, TreeReferenceHighlighter))
              case e: TreeReferencesEvent =>
                e.references.foreach((ref:STree) => highlight(ref.pos, TreeReferenceHighlighter))
              case _ =>
            }
          }
        }
        
       override def itemExited(item: VisualItem, e: MouseEvent) {
          treeTransformedViewer.setText(null)
          clearHighlight()
        }
        
       private def highlight(pos: Position, colorSelection: DefaultHighlighter.DefaultHighlightPainter) {
         if (pos.isRange) {
           //clearHighlight()
           treeViewerHighlighter.addHighlight(pos.start, pos.end, colorSelection)
         }
       }
    
       private def clearHighlight() {
         treeViewerHighlighter.getHighlights.foreach(treeViewerHighlighter.removeHighlight(_))
       }
       
       object TreeMainHighlighter extends DefaultHighlighter.DefaultHighlightPainter(Color.red)
       object TreeReferenceHighlighter extends DefaultHighlighter.DefaultHighlightPainter(Color.green)
    }

    private def loadFile(fName: String) {
      // at the moment we only ensure that there is only one
      val f = new File(fName)
      val src = if (f.exists) {
       ScalaFile(fName).slurp 
      } else "Source does not exist"
      treeGeneralViewer.setText(src)
    }
    
    // Handle action on the node of the graph.
    // Expand the node that was just clicked. Also cleanup all the intermediate nodes leading to it.
    class FixedNode(fixedGroup: String, nonGoalGroup: String, toRemoveGroup: String) extends ControlAdapter {
      override def itemClicked(item: VisualItem, e: MouseEvent) {
        if (!e.isControlDown() || !e.isShiftDown() || !item.isInstanceOf[NodeItem])
          return
        val vis = item.getVisualization
        // Fixed group always contains NodeItems
        val fGroup = vis.getFocusGroup(fixedGroup)
        if (fGroup.containsTuple(item)) {
          fGroup.removeTuple(item)
          (vis.getFocusGroup(toRemoveGroup)).addTuple(item.get(label).asInstanceOf[UINodeP].pfuseNode)
          cleanupLinkPath(item.asInstanceOf[NodeItem], vis)
        } else {
          fGroup.addTuple(item)
        }
        lastClicked = Some(item.asInstanceOf[NodeItem])
      }
      
      def cleanupLinkPath(starting: NodeItem, vis: Visualization) {
        var n = starting.get(label).asInstanceOf[UINodeP]
        val tsNonGoal = vis.getFocusGroup(nonGoalGroup)
        val tsRemove = vis.getFocusGroup(toRemoveGroup)
        while (!n.goal && n.parent.isDefined) {
          tsNonGoal.removeTuple(n.pfuseNode)
          tsRemove.addTuple(n.pfuseNode)
          n = n.parent.get
        }
      }
    }
    
    class KeyPressAddGoal() extends ControlAdapter {
      
      val validKeys = List(KeyEvent.VK_DOWN, KeyEvent.VK_UP, KeyEvent.VK_LEFT, KeyEvent.VK_RIGHT)
      override def itemKeyPressed(item: VisualItem, k: KeyEvent) = keyPressed(k)
      
      override def keyPressed(k: KeyEvent) {
        // pre-filter
        val keyCode = k.getKeyCode
        /*if (keyCode < 37 || keyCode > 40) {
          controlKeyPressed(k)
          return
        }*/
          
        val last: NodeItem = if (lastClicked.isDefined)
            lastClicked.get
          else {
            // Find bottom most event
            val vis = treeView.getVisualization
            val ts = vis.getFocusGroup(Visualization.FOCUS_ITEMS)
            val goals = vis.getFocusGroup(TreeDisplay.openGoalNodes)
            ts.tuples().find(item =>
              if (item.isInstanceOf[NodeItem]) {
                val item1 = item.asInstanceOf[NodeItem].get(label).asInstanceOf[UINodeP]
                if (item1.parent.isDefined && goals.containsTuple(item1.parent.get.pfuseNode)) false
                else true
              } else false) match {
              case Some(top: NodeItem) =>
                top.children.find(item =>
                  item.asInstanceOf[NodeItem].get(label).asInstanceOf[UINodeP].goal) match {
                  case Some(v:NodeItem) =>
                    v // there is always a goal child
                  case _ =>
                    println("[warning] cannot navigate, found bug")
                    // bug
                    return
                }
              case None =>
                treeView.treeLayout.getLayoutRoot()
                return
            }
          }

        val vis = last.getVisualization
        val vGroup = vis.getFocusGroup(Visualization.FOCUS_ITEMS)

        treeView.hoverController.clearTooltip()
        keyCode match {
          case KeyEvent.VK_DOWN =>
            // expand down (if necessary)
            val n = last.get(label).asInstanceOf[UINodeP]
            if (n.parent.isDefined)
              navigate(n.parent.get.pfuseNode, vis)
            
          case KeyEvent.VK_LEFT =>
            // expand left neighbour (if possible)
            val prevSibling = last.getPreviousSibling()
            if (prevSibling != null)
              navigate(prevSibling.get(label).asInstanceOf[UINodeP].pfuseNode, vis)
            
          case KeyEvent.VK_RIGHT =>
            // expand right neighbour (if possible)
            val nextSibling = last.getNextSibling()
            if (nextSibling != null)
              navigate(nextSibling.get(label).asInstanceOf[UINodeP].pfuseNode, vis)

          case KeyEvent.VK_UP =>
            if (last.getChildCount() > 0)
              navigate(last.getFirstChild().get(label).asInstanceOf[UINodeP].pfuseNode, vis)
              
          case other =>
            controlKeyPressed(other, last)
            
        }
      }
      
      def controlKeyPressed(k: Int, node: NodeItem) = k match {
        case KeyEvent.VK_ENTER =>
          val pNode = node.get(label).asInstanceOf[UINodeP].pfuseNode
          node.getVisualization.items(new TreeDisplay.VisualItemSearchPred(pNode, label)).toList match {
            case List(single: VisualItem) =>
              treeView.hoverController.showItemTooltip(single)
            case _ =>
          }
          
        case KeyEvent.VK_ESCAPE =>
          treeView.hoverController.clearTooltip()
        
        case _ =>
          
      }
      
      def navigate(n: Node, vis: Visualization) {
        vis.items(new TreeDisplay.VisualItemSearchPred(n, label)).toList match {
          case List(first: VisualItem) =>
            AddGoal.addClickedItem(first, vis)
            treeHighlighter.itemEntered(first, null)
            // need to schedule the action by hand since keycontrol doesn't do it
            vis.run("filter")
          case _ =>
            println("incorrect search " + n)
        }
      }
    }

    // Find node which is somehow linked (tree or symbol reference) to the
    // one that was just clicked (with Ctrl).
    // Use case: clicking on a node to see at what point it's type was set.
    class LinkNode(linkGroup: String, nodeGroup: String,
      nonGoalGroup: String, goalGroup: String)
      extends ControlAdapter {
      
      class FindNode(id: Int) extends AbstractPredicate {
        override def getBoolean(t: Tuple): Boolean = {
          if (t.isInstanceOf[NodeItem]) {
            val e = t.asInstanceOf[NodeItem]
            if (e.canGet(label, COLUMN_PREFUSENODE_CLASS)) {
              val ev = e.get(label).asInstanceOf[UINodeP].ev
              ev != null && ev.id == id
            } else false
          } else false
        }
      }
      
      def addLinkPath(starting: NodeItem, vis: Visualization) {
        var n = starting.get(label).asInstanceOf[UINodeP]
        val tsNonGoal = vis.getFocusGroup(nonGoalGroup)
        while (!n.goal && n.parent.isDefined) {
          tsNonGoal.addTuple(n.pfuseNode)
          n = n.parent.get
        }
        
        if (n.goal) {
          val tsGoal = vis.getFocusGroup(goalGroup)
          while (!tsGoal.containsTuple(n.pfuseNode)) {
            tsGoal.addTuple(n.pfuseNode)
            // better check 
            n = n.children.find(_.goal).get
          }
        }
      }
      
      override def itemClicked(item: VisualItem, e: MouseEvent) {
        if (!e.isControlDown() || e.isShiftDown() || !item.isInstanceOf[NodeItem])
          return

        val vis = item.getVisualization
        // or maybe just add to non-goal group?
        
        val node = item.asInstanceOf[NodeItem]
        val eNode = node.get(label).asInstanceOf[UINodeP]
        eNode.ev match {
          case e@IdentTyper(tree0) =>
            if (settings.debugTD.value)
              println("[Link] IdentTyper event " + tree0.symbol)
            val refId = tree0.symbol.previousHistoryEvent(e.id)
            if (refId != NoEvent.id) {
                // Find corresponding event and node in the tree
                //println("Found info in the history: " + refId)
                val ts2 = vis.items(nodeGroup, new FindNode(refId))
                val tsTarget = vis.getFocusGroup(linkGroup)
                // will ts2 return NodeItem or Node
                ts2.foreach(n => {
                  tsTarget.addTuple(n.asInstanceOf[Tuple])
                  // need to find common root with the currently visible tree
                  // go until you find goal
                  addLinkPath(n.asInstanceOf[NodeItem], vis)               
                })
            }
          case _ =>
            // Do nothing for the moment
            // we need to find other cases, where
            // we might want to link
        }
      }
    }
    
    
    // 'Stick' node that was clicked with Shift & Ctrl.
    // It will be visible even if it is not on a path to a goal(errors etc).
    class AddGoal() extends ControlAdapter {
      import AddGoal._
      override def itemClicked(item: VisualItem, e: MouseEvent) {
        if (e.isControlDown() || e.isShiftDown() || !item.isInstanceOf[NodeItem])
          return
        addClickedItem(item, item.getVisualization())
      }
    }
    
    object AddGoal {
      def addClickedItem(item: VisualItem, vis: Visualization) {
        // Add or remove from focus group
        val vis = item.getVisualization
        val ts1 = vis.getFocusGroup(TreeDisplay.openGoalNodes)
        val ts2 = vis.getFocusGroup(TreeDisplay.nonGoalNodes)
        val clicked = vis.getFocusGroup(TreeDisplay.clickedNode)

        cleanupNodesAction.clean(item)
        clicked.clear()
        clicked.addTuple(item)

        // identify parent goal
        val node = item.asInstanceOf[NodeItem]
        val eNode = node.get(label).asInstanceOf[UINodeP]
        
        // is any of its children a goal
        val hasGoalChild = node.outNeighbors().exists(n =>
         {
           val node0 = n.asInstanceOf[NodeItem].get(label).asInstanceOf[UINodeP]
           node0.goal // it has to be already expanded, so this has to be valid
         }) || eNode.goal
        if (hasGoalChild) {
          // we are dealing with a goal or its parent
          eNode.parent match {
            case Some(parent) =>
              // expand its parent
              parent.goal = true
              ts1.addTuple(parent.pfuseNode.asInstanceOf[Tuple])
            case None =>
          }
        } else {
          // we are dealing with a non-direct goal
          // expand its children which are non-goals
          var eNode0 = eNode
          // goals are all in, so we are fine 
          while (!eNode0.goal && eNode0.parent.isDefined) {
            ts2.addTuple(eNode0.pfuseNode.asInstanceOf[Tuple])
            eNode0 = eNode0.parent.get
          }
        }
        lastClicked = Some(node)        
      }
    }
    
    class CleanupAction(goalsGroup: String, nonGoalsGroup: String,
                        removeGroup: String, linkGroup: String) {
      def clean(item: VisualItem) {
        // should do the check for l ?
        if (!item.canGet(label, COLUMN_PREFUSENODE_CLASS))
          return

        var eNode = item.get(label).asInstanceOf[UINodeP]
        val vis = item.getVisualization
        val List(ts1, ts2, ts3, tsRemove) =
          List(goalsGroup, nonGoalsGroup, linkGroup, removeGroup).map(vis.getFocusGroup(_))
        
        // Remove all the link nodes
        ts3.tuples().foreach(n => tsRemove.addTuple(n.asInstanceOf[Tuple]))
        ts3.clear()
        
        if (eNode.goal) {
          // Collapse all the subgoals above
          // Currently disable messages view when dealing
          // with multiple errors (least spanning tree problem)
          if (eNode.parent.isDefined) {
            // cached minimal spanning tree
            val cached = treeView.initialNodes.minimumVisibleNodes
            var eNode0 = eNode.parent.get
            while (eNode0.parent.isDefined && ts1.containsTuple(eNode0.parent.get.pfuseNode)) {            
              eNode0 = eNode0.parent.get
              if (!cached.contains(eNode0.pfuseNode)) {
                ts1.removeTuple(eNode0.pfuseNode)
                tsRemove.addTuple(eNode0.pfuseNode)
              }
            }
          }
          
          // also collapse non-goals
          ts2.tuples().foreach(t => tsRemove.addTuple(t.asInstanceOf[Tuple]))
          ts2.clear()
        } else {
          // Remove all the other non-essential non-goals
          // apart from those leading to this node
          ts2.tuples().foreach(t => tsRemove.addTuple(t.asInstanceOf[Tuple]))
          ts2.clear()
          var eNode0 = eNode
          while(eNode0.parent.isDefined && !ts1.containsTuple(eNode0.parent.get.pfuseNode)) {
            ts2.addTuple(eNode0.pfuseNode)
            eNode0 = eNode0.parent.get            
          }
        }
      }
    }
    

    // TODO remove?
    class FocusOnNode(group: String, count: Int = 2)
      extends ControlAdapter {
      override def itemClicked(item: VisualItem, e: MouseEvent) {
        if (e.getClickCount == count) {
          // Add or remove from focus group
          val vis = item.getVisualization
          val ts = vis.getFocusGroup(group)
          if (ts != null) {
            if (ts.containsTuple(item)) {
              ts.removeTuple(item)
            } else {
              ts.addTuple(item)
            }
          }
        }
      }
    }
  }

  //TODO include settings
  def buildStructure(srcs: List[String], settings: TypeDebuggerSettings, fxn: Filter, label: String) : (Tree, List[UINodeP]) = {
    val builder = new EventTreeStructureBuilder(srcs, label)
    builder(fxn)
    // provide prefuse-specific structure
    val prefuseTree = new Tree()
    val (root, initial) = EventNodeProcessor.processTree(prefuseTree, builder.root,
                                                       builder.initialGoals, label)

    if (settings.debugTD.value)
      println("[errors] " + initial.map(_.ev))

    if (settings.fullTypechecking.value) (prefuseTree, Nil) else (prefuseTree, initial)
  }

  class SwingViewer {
    def browse(srcs: List[String], settings: TypeDebuggerSettings) {
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
                                                    // indendation needs to be separated from filtering stuff
                                                    // ATM opening/closing events cannot be filtered out at this point
      }, EVDSL.ph <= 4)

      val NODESLABEL = "event.node"  // TODO
      val (prefuseTree, goals) = buildStructure(srcs, settings, filtr, NODESLABEL)
      val frame = new TypeDebuggerFrame(prefuseTree, srcs, NODESLABEL, goals)
      val lock = new Lock()
      frame.createFrame(lock)
     
      lock.acquire
    }
  }

}
