package scala.typedebugger


import java.awt.{List => awtList, _}
import java.awt.geom.{Point2D, Rectangle2D}
import java.awt.event._
import javax.swing.{Action => swingAction, _}
import javax.swing.event.TreeModelListener

import javax.swing.text.{Highlighter, DefaultHighlighter}

import scala.concurrent.Lock
import scala.collection.mutable.{ ListBuffer, Stack }
import scala.collection.mutable. { HashMap }
import scala.tools.nsc.{ Global, CompilerCommand, Settings, symtab, io }
import scala.tools.nsc.reporters.{ ConsoleReporter }


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

import scala.collection.JavaConversions._

import java.io.File

abstract class TypeBrowser {
  val global: Global
 
  import global.{Tree => STree, _}
  import EV._

  object CompileWrapper {
    private def sources(roots: List[String]): List[String] = {
      val lb = new ListBuffer[String]
      roots foreach { p =>
        io.Path(p).walk foreach { x =>
          if (x.isFile && x.hasExtension("scala", "java"))
            lb += x.path
        }
      }
      lb.toList.distinct
    }
  
    def cc(paths: List[String]): Boolean = {
      val run = new Run
      val srcs = sources(paths)
      println("[compiling] " + srcs)
      run compile srcs
      !reporter.hasErrors
    }
  }

  object TypeBrowserFrame {
    val BACKGROUND = Color.WHITE
    val FOREGROUND = Color.BLACK
    val COLUMN_CLASS = ETreeNode(null, new ListBuffer(), None, null).getClass
  }

  object TreeView {
    val tree = "tree"
    val treeNodes = "tree.nodes"
    val treeEdges = "tree.edges"
    val typecheckerNodes = "tree.typechecker"
    //val orientation = Constants.ORIENT_LEFT_RIGHT
    val orientation = Constants.ORIENT_BOTTOM_TOP

    val fixedNodes = "tree.fixed"
    val openGoalNodes = "tree.openGoals"
    val nonGoalNodes = "tree.openNods"
    val toRemoveNodes = "tree.removeNodes"
    val linkGroupNodes = "tree.link"
    val clickedNode = "tree.clicked"
  }

  class TreeView(t: Tree, label: String, initialGoals: List[ETreeNode])
    extends Display(new Visualization()) with ui.PrefuseTooltips {
    
    treeViewSelf => 
    
    import TreeView._
    
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
    //filter.add(new FocusOnItems(Visualization.FOCUS_ITEMS,
    //                            ErrorPredicate))
    //filter.add(new FocusOnItems(Visualization.FOCUS_ITEMS,
    //                            new FixedNodesPredicate(fixedNodes)))
    //filter.add(new FisheyeTreeFilter(tree, 1))
    filter.add(new VisualizeFixedNodes(fixedNodes, nonGoalNodes))
    filter.add(new UnfocusOnItems(Visualization.FOCUS_ITEMS, toRemoveNodes))
    filter.add(new FocusOnItems(Visualization.FOCUS_ITEMS,
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

    //m_vis.putAction("initial-focus", new InitialFocusOnErrors(Visualization.FOCUS_ITEMS))
    //m_vis.putAction("initial-goals", new VisibilityFilter(InitialGoalPredicate))
    val initialNodes = new InitialCollapseOnGoal(Visualization.ALL_ITEMS,
                                                 openGoalNodes,
                                                 clickedNode)
    m_vis.putAction("initial-goals", initialNodes)
 
    val zoomToFit = new ZoomToFitControl()
    zoomToFit.setZoomOverItem(false)
    // initialize the display
    setSize(700,800)
    setItemSorter(new TreeDepthItemSorter())
    addControlListener(zoomToFit)
    addControlListener(new ZoomControl())
    addControlListener(new WheelZoomControl())
    addControlListener(new PanControl())
    addControlListener(new HoverTooltip())
    addControlListener(new FocusControl(1, "filter"))

    setOrientation(orientation)
    m_vis.addFocusGroup(fixedNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(openGoalNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(nonGoalNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(toRemoveNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(linkGroupNodes, new DefaultTupleSet())
    m_vis.addFocusGroup(clickedNode, new DefaultTupleSet())

    m_vis.run("initial-goals")
    m_vis.run("filter")
    //
    //m_vis.run("initial-focus")
    

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
        if (item.canGet(label0, TypeBrowserFrame.COLUMN_CLASS)) {
          val eNode = item.get(label0).asInstanceOf[ETreeNode]
          eNode.ev.eventString
        } else null
      }
    }
    
    class CustomNodeLinkTreeLayout(wholeTree: String, visGroup: String,
      orientation: Int, dspace: Double, bspace: Double, tspace: Double)
      extends NodeLinkTreeLayout(wholeTree) {
      
      object GoalNode extends AbstractPredicate {
        override def getBoolean(t: Tuple): Boolean = {
          if (t.canGet(label, TypeBrowserFrame.COLUMN_CLASS) && t.isInstanceOf[NodeItem]) {
            // because we added nodeItem to the list, not visualItem which 't' is
            val nodeItem = t.get(label).asInstanceOf[ETreeNode]
            nodeItem.goal && t.asInstanceOf[NodeItem].isVisible
          } else false
        }
      }
      
      class FirstNodeFallback(search: Node) extends AbstractPredicate {
        override def getBoolean(t: Tuple): Boolean = {
          if (t.canGet(label, TypeBrowserFrame.COLUMN_CLASS) && t.isInstanceOf[NodeItem]) {
            // because we added nodeItem to the list, not visualItem which 't' is
            val nodeItem = t.get(label).asInstanceOf[ETreeNode]
            nodeItem.pfuseNode == search
          } else false
        }
      } 
      
      override def getLayoutRoot(): NodeItem = {
        // use initialGoals information
        // for now just one and go upwards
        val allVisibleGoals = m_vis.items(visGroup, GoalNode)
        val allPNodeVisibleGoals = allVisibleGoals.map(t => {
          val t0 = t.asInstanceOf[NodeItem]
          (t0.get(label).asInstanceOf[ETreeNode].pfuseNode, t0)
        }).toMap
        
        initialGoals match {
          case head::_ =>
            // Need to find respective VisualItem for node so that
            // we can match prefuse node stored in ETreeNode
            var eNode = head 
            while (eNode.parentENode.isDefined && allPNodeVisibleGoals.contains(eNode.parentENode.get.pfuseNode)) {
              eNode = eNode.parentENode.get
            }
            if (!allPNodeVisibleGoals.contains(eNode.pfuseNode)) {
              // we are dealing with a first node
              // so try to find it manually
              val first = m_vis.items(wholeTree, new FirstNodeFallback(head.pfuseNode))
              if (first.hasNext)
                first.next.asInstanceOf[NodeItem]
              else {
                //println("FALLBACK")
                super.getLayoutRoot()
              }
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


    // Some predefined actions (direct translation from the example)
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
      
      /*override def itemEntered(item: VisualItem, e: MouseEvent) {
        if(item.isInstanceOf[NodeItem]) {
          showNodeTooltip(item, e)
        }
      }*/
      
      override def itemPressed(item: VisualItem, e: MouseEvent) {
        if(activeTooltip != null)
          activeTooltip.stopShowingImmediately()
      }
      
      override def itemReleased(item: VisualItem, e: MouseEvent) {
        if(item.isInstanceOf[NodeItem] && e.getButton() == MouseEvent.BUTTON3)
          showNodeTooltip(item, e)
      }
      
      protected def showNodeTooltip(item: VisualItem, e: MouseEvent) {
        val v = item.getVisualization()
        val eNode = item.get(label).asInstanceOf[ETreeNode]
        
        showTooltip(new NodeTooltip("Some name",
          eNode.fullInfo,
          100, 100, e.getSource().asInstanceOf[Display]),
            item, e)
      }
      
      private def showTooltip(ptt: PrefuseTooltip, item: VisualItem, e: MouseEvent) {
        if(activeTooltip != null) {
          activeTooltip.stopShowingImmediately()
        }
        
        activeTooltip = ptt
        activeTooltip.startShowing(e.getX() + 10, e.getY() + 5,
            (getWidth()/2) < e.getX(),
            (getHeight()/2) < e.getY())
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
        if (item.canGet(label, TypeBrowserFrame.COLUMN_CLASS))
          Some(item.get(label).asInstanceOf[ETreeNode].ev)
        else
          None

      // TODO: events should have appropriate colors
      override def getColor(item: VisualItem): Int = {
        val event = retrieveEvent(item)
        event match {
          case Some(ev: HardErrorEvent) =>
            ColorLib.rgba(255, 0, 0, 150)
          case Some(ev: SoftErrorEvent) =>
            ColorLib.rgba(255, 0, 0, 50)
          case _ =>
            // search currently not supported
            if ( m_vis.isInGroup(item, Visualization.SEARCH_ITEMS) )
              ColorLib.rgb(255,190,190)
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



    class FocusOnItems(visGroup: String, predicate: Predicate) extends Action{
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
    
    // Remove given nodes from toRemoveGoals group 
    class UnfocusOnItems(visGroup: String, toRemoveGoals: String) extends Action{
     
      class ToRemovePredicate(toRemoveGroup: String) extends AbstractPredicate {
        override def getBoolean(t: Tuple): Boolean = {
        val ts = m_vis.getFocusGroup(toRemoveGroup)
  
          if (ts != null && t.canGet(label, TypeBrowserFrame.COLUMN_CLASS)) {
            // because we added nodeItem to the list, not visualItem which 't' is
            val nodeItem = t.get(label).asInstanceOf[ETreeNode].pfuseNode
            ts.containsTuple(nodeItem)
          } else false
        }
      }
      
      private val pred = new ToRemovePredicate(toRemoveGoals)
      
      def run(frac: Double) {
        import scala.collection.JavaConversions._
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
    
    
    // Add all nodes from groupName to the vis
    class VisualizeNodes(visGroup: String, groupName: String) extends Action {
      def run(frac: Double) {
        val target = m_vis.getFocusGroup(visGroup)
        val ts = m_vis.getFocusGroup(groupName)
        ts.tuples().foreach(n => target.addTuple(n.asInstanceOf[Tuple]))
      }
    }
    
    class VisualizeFixedNodes(fixedGroup: String, nonGoalGroup: String)
      extends VisualizeNodes(fixedGroup, "") {
      override def run(frac: Double) {
        val target = m_vis.getFocusGroup(fixedGroup)
        target.tuples().foreach(n =>
          addLinkPath(n.asInstanceOf[NodeItem]))
      }
      
      def addLinkPath(starting: NodeItem) {
        var n = starting.get(label).asInstanceOf[ETreeNode]
        val tsNonGoal = m_vis.getFocusGroup(nonGoalGroup)
        while (!n.goal && n.parentENode.isDefined) {
          tsNonGoal.addTuple(n.pfuseNode)
          n = n.parentENode.get
        }
      }
    }
    
    // need to show all the goals, all edges between them, 
    // as well as immediate (1-distance) subgoals of each goal
    class ShowAllGoalsAndEdges(visGroup: String, clickedNode: String) extends Action {
      
      def run(frac: Double) {
        import scala.collection.JavaConversions._
        val ts = m_vis.getFocusGroup(visGroup)
        
        if (ts.getTupleCount() == 0) {
          val root = treeLayout.getLayoutRoot()
          ts.addTuple(root)
          val clickedTs = m_vis.getFocusGroup(clickedNode)
          clickedTs.addTuple(root)
        }
        
        for (item <- ts.tuples()) {
          // Each of the goals
          //println("show goal: " + item)
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
              val eNode = item0.get(label).asInstanceOf[ETreeNode]
              if (!eNode.goal)
                item0.inEdges().foreach(edge =>
                  PrefuseLib.updateVisible(edge.asInstanceOf[VisualItem], true)
                )
            case _ =>
          }
        }
        

        
      }
    }

    // Set focus window on errors first
    class InitialFocusOnErrors(group: String) extends Action {
      private val margin = 50
      private val duration = 100
 
      def run(frac: Double) {
        if (m_vis.getDisplayCount != 1)
          println("[Warning] Multiple displays detected! " + m_vis.getDisplayCount)

        val ts = m_vis.getFocusGroup(group).tuples
        if (!ts.isEmpty) {
          // Adapt bounds
          val bounds = m_vis.getBounds(group)
          val display = m_vis.getDisplay(0)
          //println("Current bounds " + bounds)
//          val tsMap: Iterator[VisualI] = ts.map(_.asInstanceOf[VisualItem]).t
          //println(ts.map(_.toString).mkString(","))
          GraphicsLib.expand(bounds, margin + (1/display.getScale))
          //println("New Bounds: " + bounds)
          val boundsss = ts.map(_.asInstanceOf[VisualItem]).toList
          //println("Different boundss: " + boundsss)
          val cx = bounds.getCenterX
          val cy = bounds.getCenterY
          val center = new Point2D.Double(cx,cy)
          //DisplayLib.fitViewToBounds(display, bounds, new Point2D.Double(cx,cy), 0)
          // TODO: this should properly calculate the bounds, but for some reason
          // it fails
          display.panToAbs(center)
          display.zoomAbs(center, calculateScale(display, center, bounds))
        }
      }
      
      //TODO: fix properly 
      private def calculateScale(display: Display, center: Point2D, bounds: Rectangle2D): Double = {
        val w = display.getWidth()
        val h = display.getHeight()
        
        // compute half-widths of final bounding box around
        // the desired center point
        val wb = Math.max(center.getX-bounds.getMinX(),
                             bounds.getMaxX()-center.getX)
        val hb = Math.max(center.getY-bounds.getMinY(),
                             bounds.getMaxY()-center.getY)
        
        // compute scale factor
        //  - figure out if z or y dimension takes priority
        //  - then balance against the current scale factor
        //println("Half widths: " + wb + " " + hb + " for " + bounds)
        val scale = Math.min(w/(2*wb),h/(2*hb)) / display.getScale()
        //println("display width " + w + " " + h)
        //println("scala::: " + display.getScale + " " + scale)
        //println("True size: " + treeViewSelf.getSize())
        
        //scale
        1
      }

    }
    
    

    
    class GoalPathPredicate(openGoalsGroup: String, nonGoalsGroup: String) extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = {
        


        if (t.canGet(label, TypeBrowserFrame.COLUMN_CLASS)) {
          val ts = m_vis.getFocusGroup(openGoalsGroup)
          // because we added nodeItem to the list, not visualItem which 't' is
          val nodeItem = t.get(label).asInstanceOf[ETreeNode].pfuseNode
          val res0 = ts != null && ts.containsTuple(nodeItem)
          val res = if (res0) {
//            println("adding goal: " + nodeItem)
            res0
          } else {
            // try non goal group
            val ts2 = m_vis.getFocusGroup(nonGoalsGroup)
            val res1 = ts2 != null && ts2.containsTuple(nodeItem)
//            if (res1)
//              println("adding non-goal : " + nodeItem)
            res1
          }
          res
        } else false
      }
    }
    



    object ErrorPredicate extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = {
        if (t.isInstanceOf[NodeItem]) {
          val e = t.asInstanceOf[NodeItem]
          if (e.canGet(label, TypeBrowserFrame.COLUMN_CLASS)) {
            val ev = e.get(label).asInstanceOf[ETreeNode].ev
            // Automatic expand for all errors
            // TODO: make this configurable
            ev.isInstanceOf[HardErrorEvent] || ev.isInstanceOf[SoftErrorEvent]
          } else false
        } else false
      }
    }

    class FixedNodesPredicate(fixedGroup: String) extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean = {
        val ts = m_vis.getFocusGroup(fixedGroup)
        ts != null && ts.containsTuple(t)
      }
    }

    object InformationEdgePredicate extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean =
        if (t.isInstanceOf[Edge]) {
          val e = t.asInstanceOf[Edge]
          if (e.getTargetNode.canGet(label, TypeBrowserFrame.COLUMN_CLASS)) {
            val n = e.getTargetNode.get(label).asInstanceOf[ETreeNode]
            !n.ev.blockStart
          } else false
        } else false
    }
    
    // Predicate for checking if an edge is between the two goals
    class MainGoalPathEdgePredicate(goalsGroup:String) extends AbstractPredicate {
      override def getBoolean(t: Tuple): Boolean =
        if (t.isInstanceOf[EdgeItem]) {
          val e = t.asInstanceOf[EdgeItem]
          if (e.getSourceNode.canGet(label, TypeBrowserFrame.COLUMN_CLASS)) {
            val tNode = e.getTargetNode.get(label).asInstanceOf[ETreeNode]
            val sNode = e.getSourceNode.get(label).asInstanceOf[ETreeNode]
            tNode.goal && sNode.goal
            //val vis = e.getVisualization()
            //val ts = vis.getFocusGroup(goalsGroup)
            
            //ts.containsTuple(tNode.pfuseNode) && ts.containsTuple(sNode.pfuseNode)
          } else false
        } else false
    }

    // Collapse the whole tree initially so that only (hard) errors are visible
    class InitialCollapseOnGoal(group: String, openGoalsGroup: String, clickedNode: String)
      extends Action {
      
      val toExpand = new ListBuffer[NodeItem]()
      
      object InitialGoalPredicate extends AbstractPredicate {
        private def isInitialGoal(node: ETreeNode) =
          node.ev.isInstanceOf[HardErrorEvent] && initialGoals.contains(node)
        
        override def getBoolean(t: Tuple): Boolean = {
          if (t.isInstanceOf[NodeItem]) {
            val e = t.asInstanceOf[NodeItem]
            if (e.canGet(label, TypeBrowserFrame.COLUMN_CLASS)) {
              val eNode = e.get(label).asInstanceOf[ETreeNode]
  
              // this will need to be adapted
              if (!isInitialGoal(eNode)) {
                if (eNode.evs.exists(isInitialGoal))
                  toExpand.add(t.asInstanceOf[NodeItem])
                false
              } else true
            } else false
          } else false
        }
      }
      
      private def setGoalPath(eNode: Option[ETreeNode]) {
        eNode match {
          case Some(n) if !n.goal =>
            n.goal = true
            setGoalPath(n.parentENode)
          case _ =>
        }   
      }
      
      // TODO: design more clever algorithm 
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
          // increase the counter 
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
      
      lazy val minimumVisibleNodes: List[Node] = {
        findLeastCommonSpanningTree(allInitialGoals)
      }

      def run(frac: Double) {
        val items = m_vis.items(group)
        val ts = m_vis.getFocusGroup(openGoalsGroup)
        //val font = new Font("monospaced", Font.PLAIN, 14)
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
            if (item0.canGet(label, TypeBrowserFrame.COLUMN_CLASS)) {
              val eNode = item0.get(label).asInstanceOf[ETreeNode]
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
          ts.addTuple(nItem.get(label).asInstanceOf[ETreeNode].pfuseNode)
        })
      }
    }
  }

  class TypeBrowserFrame(t: Tree, srcs: List[String], l: String, goals: List[ETreeNode]) {

    import TypeBrowserFrame._
    val frame = new JFrame("Type Debugger 0.0.4")
    val topPane = new JPanel(new BorderLayout())

    val treeTransformedViewer = new JTextArea(30, 90)
    val treeGeneralViewer = new JTextArea(30, 30)
    val treeHighlighter = treeGeneralViewer.getHighlighter()
    
    private val cleanupNodesAction = new CleanupAction(TreeView.openGoalNodes,
                                                       TreeView.nonGoalNodes,
                                                       TreeView.toRemoveNodes,
                                                       TreeView.linkGroupNodes)
    
    val treeView = new TreeView(t, l, goals)

    // Displays all info related to the specific event
    // TODO: we should customize it at this point,
    // possibly by handling every event here and extracting necessary info
    // instead of redirecting to Event.eventString
//    val contextInfoArea = new JTextArea(30, 120) // TODO introduce more interactive widget

    def createFrame(lock: Lock): Unit = {
      lock.acquire // keep the lock until the user closes the window
      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      frame.addWindowListener(new WindowAdapter() {
        override def windowClosed(e: WindowEvent): Unit = lock.release
      })

      val tabFolder = new JTabbedPane()

      
      treeView.setBackground(BACKGROUND)
      treeView.setForeground(FOREGROUND)

//      val splitPane =
//        new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPane, bottomPane)
//      splitPane.setResizeWeight(0.5)


 
      // Split right part even further
      val topSplitPane = 
        new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
          treeView,
          new JScrollPane(tabFolder))
      topSplitPane.setResizeWeight(0.7)
 

      topPane.add(topSplitPane)

//      bottomPane.add(new JScrollPane(contextInfoArea))

     

//      bottomPane.add(new JScrollPane(textArea), BorderLayout.CENTER)
//      textArea.setFont(new Font("monospaced", Font.PLAIN, 14))
//      textArea.setEditable(false)

      tabFolder.addTab("Tree", null,
        new JScrollPane(treeGeneralViewer))
//      val gTree = new TabItem(tabFolder, SWT.NONE)
//      gTree.setText("Tree")
//      gTree.setControl(treeTransformedViewerArea)

//      val tTree = new TabItem(tabFolder, SWT.NONE)
      tabFolder.addTab("Transformed tree", null,
        new JScrollPane(treeTransformedViewer))
//      tTree.setText("Transformed tree")
//      tTree.setControl(treeGeneralViewerArea)
//      tabFolder.setSelection(2) 


//      contextInfoArea.setFont(new Font("monospaced", Font.PLAIN, 14))
  //    contextInfoArea.setEditable(false)


      // Add listeners
      treeView.addControlListener(new ControlAdapter() {
        override def itemClicked(item: VisualItem, e: MouseEvent) {
          if (item.canGet(l, COLUMN_CLASS)) {
            val node = item.get(l).asInstanceOf[ETreeNode]
//            contextInfoArea.setText(node.fullInfo)
//            println("Item: " + item)
          }
        }

        override def itemEntered(item: VisualItem, e: MouseEvent) {
          if (item.canGet(l, COLUMN_CLASS)) {
            val node = item.get(l).asInstanceOf[ETreeNode]
            node.ev match {
	            case e:TreeEvent =>
//                println("Enter item")
                val prettyTree = asString(e.tree)
                treeTransformedViewer.setText(prettyTree)

                // Color the specific part of the general tree
//                val pos0:Position = e.tree.pos
//                val (s,end) = if (pos0.isRange) (pos0.start, pos0.end) else (0,0)

//                treeGeneralViewer.setText(pos0 + " range: " + s + " - " + end)
//               println("hightlight in : " + e.tree.pos)
                // Fix highlighting
                
                highlight(e.tree.pos, TreeMainHighlighter)
	            case e: SymEvent =>
	              // ensure not TreeEvent as well?
	              highlight(e.sym.pos, TreeMainHighlighter)
              case _ =>
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
//          println("Item : " + item + " [out]")
          clearHighlight()
        }
      })
      //treeView.addControlListener(new FocusOnNode(TreeView.fixedNodes))
      treeView.addControlListener(new AddGoal(TreeView.openGoalNodes,
                                              TreeView.nonGoalNodes,
                                              TreeView.clickedNode))
      treeView.addControlListener(new LinkNode(TreeView.linkGroupNodes,
                                               TreeView.treeNodes,
                                               TreeView.nonGoalNodes,
                                               TreeView.openGoalNodes))
      treeView.addControlListener(new FixedNode(TreeView.fixedNodes,
                                                TreeView.nonGoalNodes,
                                                TreeView.toRemoveNodes))


      //splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topSplitPane, bottomPane)
      if (srcs.isEmpty)
        println("[Warning] No files specified for debugging.")
      loadFile(srcs.head)
//      frame.getContentPane().add(splitPane)
      frame.getContentPane().add(topPane)
      frame.pack()
      frame.setVisible(true)
    }

    private def highlight(pos: Position, colorSelection: DefaultHighlighter.DefaultHighlightPainter) {
      if (pos.isRange) {
        //clearHighlight()
        treeHighlighter.addHighlight(pos.start, pos.end, colorSelection)
      }
    }
    private def clearHighlight() {
        treeHighlighter.getHighlights.foreach(treeHighlighter.removeHighlight(_))
    }
    private def loadFile(fName: String) {
      // at the moment we only ensure that it is only one
      val f = new File(fName)
      val src = if (f.exists) {
       io.File(fName).slurp 
      } else "Source does not exist"
      treeGeneralViewer.setText(src)
    }

    
    
    object TreeMainHighlighter extends DefaultHighlighter.DefaultHighlightPainter(Color.red)
    object TreeReferenceHighlighter extends DefaultHighlighter.DefaultHighlightPainter(Color.green)
    
    class FixedNode(fixedGroup: String, nonGoalGroup: String, toRemoveGroup: String) extends ControlAdapter {
      override def itemClicked(item: VisualItem, e: MouseEvent) {
        if (!e.isControlDown() || !e.isShiftDown())
          return
        //
          
        if (!item.isInstanceOf[NodeItem])
          return
          
        val vis = item.getVisualization
        // Fixed group always contains nodeitem
        val fGroup = vis.getFocusGroup(fixedGroup)
        if (fGroup.containsTuple(item)) {
          fGroup.removeTuple(item)
          //fGroup.removeTuple(item.get(l).asInstanceOf[ETreeNode].pfuseNode)
          val tsRemove = vis.getFocusGroup(toRemoveGroup)
          tsRemove.addTuple(item.get(l).asInstanceOf[ETreeNode].pfuseNode)
          cleanupLinkPath(item.asInstanceOf[NodeItem], vis)
        } else
          fGroup.addTuple(item)
        
      }
      
      def cleanupLinkPath(starting: NodeItem, vis: Visualization) {
        var n = starting.get(l).asInstanceOf[ETreeNode]
        val tsNonGoal = vis.getFocusGroup(nonGoalGroup)
        val tsRemove = vis.getFocusGroup(toRemoveGroup)
        while (!n.goal && n.parentENode.isDefined) {
          tsNonGoal.removeTuple(n.pfuseNode)
          tsRemove.addTuple(n.pfuseNode)
          n = n.parentENode.get
        }
      }
    }

    class LinkNode(linkGroup: String, nodeGroup: String,
      nonGoalGroup: String, goalGroup: String)
      extends ControlAdapter {
      
      class FindNode(id: Int) extends AbstractPredicate {
        override def getBoolean(t: Tuple): Boolean = {
          if (t.isInstanceOf[NodeItem]) {
            val e = t.asInstanceOf[NodeItem]
            if (e.canGet(l, TypeBrowserFrame.COLUMN_CLASS)) {
              val ev = e.get(l).asInstanceOf[ETreeNode].ev
              ev != null && ev.id == id
            } else false
          } else false
        }
      }
      
      def addLinkPath(starting: NodeItem, vis: Visualization) {
        var n = starting.get(l).asInstanceOf[ETreeNode]
        val tsNonGoal = vis.getFocusGroup(nonGoalGroup)
        while (!n.goal && n.parentENode.isDefined) {
          tsNonGoal.addTuple(n.pfuseNode)
          n = n.parentENode.get
        }
        
        if (n.goal) {
          val tsGoal = vis.getFocusGroup(goalGroup)
          while (!tsGoal.containsTuple(n.pfuseNode)) {
            tsGoal.addTuple(n.pfuseNode)
            n.evs.find(_.goal) match {
              case Some(n0) => n = n0
              case _ => // not possible to have infinite loop
            }
          }
        }
      }
      
      override def itemClicked(item: VisualItem, e: MouseEvent) {
        if (!e.isControlDown() || e.isShiftDown())
          return

        val vis = item.getVisualization
        // or maybe just add to non-goal group?
        
        
        // TODO can we click on edge for example?
        val node = item.asInstanceOf[NodeItem]
        val eNode = node.get(l).asInstanceOf[ETreeNode]
        eNode.ev match {
          case e@IdentTyper(tree0) =>
            //println("Ident typer: " + tree0.symbol)
            val refId = tree0.symbol.previousHistoryEvent(e.id)
            if (refId != noEventId) {
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
    
    
    class AddGoal(goalGroup: String, nonGoalGroup: String, clickedNode: String)
      extends ControlAdapter {
      override def itemClicked(item: VisualItem, e: MouseEvent) {
        if (e.isControlDown() || e.isShiftDown() || !item.isInstanceOf[NodeItem])
          return
        
        // Add or remove from focus group
        val vis = item.getVisualization
        val ts1 = vis.getFocusGroup(goalGroup)
        val ts2 = vis.getFocusGroup(nonGoalGroup)
        val clicked = vis.getFocusGroup(clickedNode)
        
        cleanupNodesAction.clean(item)
        clicked.clear()
        clicked.addTuple(item)
          
        // identify parent goal
        val node = item.asInstanceOf[NodeItem]
        val eNode = node.get(l).asInstanceOf[ETreeNode]
        
        // is any of its children a goal
        val hasGoalChild = node.outNeighbors().exists(n =>
         {
           val node0 = n.asInstanceOf[NodeItem].get(l).asInstanceOf[ETreeNode]
           node0.goal // it has to be already expanded, so this has to valid
         }) || eNode.goal
        if (hasGoalChild) {
          // we are dealing we a goal
          // expand its parent
          eNode.parentENode match {
            case Some(parent) =>
              parent.goal = true
              ts1.addTuple(parent.pfuseNode.asInstanceOf[Tuple])
            case None =>
          }
        } else {
          //println("Expand non-goal: " + item)
          // we are dealing we a non-direct goal
          // expand its children which are non-goals
          var eNode0 = eNode
          // goals are all in, so we are fine 
          while (!eNode0.goal && eNode0.parentENode.isDefined) {
            ts2.addTuple(eNode0.pfuseNode.asInstanceOf[Tuple])
            eNode0 = eNode0.parentENode.get
          }
        }
        
      //  cleanupNodesAction.clean(item)
      }
    }
    
    class CleanupAction(goalsGroup: String, nonGoalGroup: String,
                        removeGroup: String, linkGroup: String) {
      def clean(item: VisualItem) {
        // should do the check for l ?
        if (!item.canGet(l, COLUMN_CLASS))
          return
        var eNode = item.get(l).asInstanceOf[ETreeNode]
        val vis = item.getVisualization
        val ts1 = vis.getFocusGroup(goalsGroup)
        val ts2 = vis.getFocusGroup(nonGoalGroup)
        val ts3 = vis.getFocusGroup(linkGroup)
        
        // Nodes to remove from the display
        val tsRemove = vis.getFocusGroup(removeGroup)
        
        // Remove all the link nodes
        ts3.tuples().foreach(n => tsRemove.addTuple(n.asInstanceOf[Tuple]))
        ts3.clear()
        
        if (eNode.goal) {
          // Collapse all the subgoals above
          // Currently disable// messases view when dealing
          // with multiple errors (least spanning tree problem)
          // TODO: re-enable
          if (eNode.parentENode.isDefined) {
            // cached minimal spanning tree
            val cached = treeView.initialNodes.minimumVisibleNodes
            var eNode0 = eNode.parentENode.get
            while (eNode0.parentENode.isDefined && ts1.containsTuple(eNode0.parentENode.get.pfuseNode)) {            
              eNode0 = eNode0.parentENode.get
              if (!cached.contains(eNode0.pfuseNode)) {
                ts1.removeTuple(eNode0.pfuseNode)
                tsRemove.addTuple(eNode0.pfuseNode)
              }
            }
          }
          
          // also collapse the non-goals?
          ts2.tuples().foreach(t => tsRemove.addTuple(t.asInstanceOf[Tuple]))
          ts2.clear()
        } else {
          // Remove all the other non-essential non-goals
          // apart from those leading to this node
          ts2.tuples().foreach(t => tsRemove.addTuple(t.asInstanceOf[Tuple]))
          ts2.clear()
          var eNode0 = eNode
          while(eNode0.parentENode.isDefined && !ts1.containsTuple(eNode0.parentENode.get.pfuseNode)) {
            ts2.addTuple(eNode0.pfuseNode)
            eNode0 = eNode0.parentENode.get            
          }
          //assert(, "Finished on parent which isn't a goal, fail." + eNode)
        }
      }
    }
    

    
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

  // Wrapper around the compiler that logs all the events
  // and creates the necessary structure
  class EventTreeStructureBuilder(srcs: List[String], nodesLabel: String) {
    var root: ETreeNode = _
    
    private val currentNodes = new Stack[(ETreeNode, Int)]()
    var previousLevel: Int = -1 // Start at root
    private var focusNodes: List[ETreeNode] = Nil

    // underlying structure necessary to build graph for prefuse library
    var t: Tree = _
    var hook: Hook.IndentationHook = _

    initTables()
    
    def initialGoals: List[ETreeNode] = focusNodes.reverse

    private def initTables() {
      t = new Tree()
      val nodes = t.getNodeTable()

      // Dummy for class, not companion object
      val dummy = ETreeNode(null, new ListBuffer(), None, null).getClass
      nodes.addColumn(nodesLabel, dummy)
    }

    // Create the graphical node and add relationships
    // Note: we do not always want to define relationship for end-blocks events
    // This would basically mean not showing them
    private def createNode(ev: Event, parentENode: ETreeNode): ETreeNode = {
      import ETreeNode.emptyETreeNode
      val prefuseNode = if (parentENode == null) {
        t.addRoot()
      } else {
        ev match {
          case _: TyperTypeSet => 
            // type was already set for tree event
            null
          case _: TyperTyped1Done => // typing event
            null
          case _: NamerDone => // bug in compiler? _:NamerDone.type crashes
            null
// TODO re-enable
//          case _: ImplicitSearchDone =>
//            null
          case _: TyperTypedDone =>
            null
          case _: TyperDone =>
            null
//          case _: AdaptDone =>
//            null
          case _                  =>
            t.addChild(parentENode.pfuseNode)
        }
      }
            
      val parentNodeInfo = if (parentENode == null) None else Some(parentENode)
      val evNode = emptyETreeNode(ev, parentNodeInfo, prefuseNode)
      
      // We want them in order of appearance
      ev match {
        case _: HardErrorEvent =>
          focusNodes = evNode::focusNodes
        case _ =>
      }
      
      
      if (prefuseNode != null) prefuseNode.set(nodesLabel, evNode)
      evNode
    }
 
    // analyze the logged events and build necessary structure for the tree
    // TODO make ev implicit
    def reportWithLevel(ev: Event, level: Int) {
      // This relies on the fact that done blocks are not filtered out
//      assert(previousLevel <= level, "prev: " + previousLevel + " level " + level)
      implicit def nodeWithLevel(a: ETreeNode): (ETreeNode, Int) = (a, level)
      implicit def onlyNode(a: Tuple2[ETreeNode, Int]): ETreeNode = a._1
      ev match {
        case _ if previousLevel < level => // instead use level indication
          val top: ETreeNode = currentNodes.top

          if (top.evs.isEmpty) {
            top.evs += createNode(ev, top)
          } else {
            val last = top.evs.last
            ev match {
              case _: DoneBlock =>
                // Don't push if it is at the same time a done block
              case _            =>
                currentNodes.push(last)
            }
             
            //assert(last.evs == Nil, "Last is not Nil: " + last.evs.mkString(",") + " want to add " + ev + " " + ev.getClass)
            last.evs += createNode(ev, last)
          }
          previousLevel = level

        case _: DoneBlock  =>
          assert(!currentNodes.isEmpty,
                  "stack of current nodes cannot be empty on end of the block for " + ev + " " + ev.getClass)
          val top: ETreeNode = currentNodes.pop()
          ev match {
            // Events that are not to be shown at all
//            case _: TyperTypedDone =>
            case _ =>
              top.evs += createNode(ev, top)
          }
          
          // perform filtering
          postFilter(top)
        
        case _             =>
          ev match {
            // TODO: After we eliminate exceptions in the compiler, this shouldn't be necessary
            case _: SecondTryTypedApplyStartTyper =>
              // rewind to TryTypedApplyTyper as we encountered error
              //val nStack = currentNodes.dropWhile(!_.ev.isInstanceOf[TryTypedApplyEvent])

              // could use dropWhile, but then we have to assign the stack anyway
              while(!currentNodes.top._1.ev.isInstanceOf[TryTypedApplyEvent])
                currentNodes.pop()
                

              val baseLevel = currentNodes.top._2 + 1
              previousLevel = baseLevel
              hook.resetIndentation(baseLevel)
            case _ =>
              previousLevel = level
          }
          // this also handles the case when we just parsed DoneBlock

          assert(!currentNodes.isEmpty)
          val top: ETreeNode = currentNodes.top
          top.evs += createNode(ev, top)
      }
    }
    
    private def postFilter(n: ETreeNode) {
      val toLeave = n.evs.filter {
        child =>
          if (filterOutStructure(child)) {
            t.removeChild(child.pfuseNode)
            false
          } else true
      }
      if (n.evs.length != toLeave.length)
        n.evs = toLeave
    }
    
    private def filterOutStructure(node: ETreeNode): Boolean = {
      node.ev match {
        case _: AdaptStart =>
          node.evs.length == 2 &&
          node.evs.get(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
          node.evs.get(1).ev.isInstanceOf[AdaptDone] ||
          node.evs.length == 1 && node.evs.head.ev.isInstanceOf[AdaptDone] 
        case _: PolyTpeAdapt =>
          node.evs.length == 2 &&
          node.evs.get(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
          node.evs.get(1).ev.isInstanceOf[AdaptDone]
        case _: TypeTreeAdapt =>
          node.evs.length == 2 &&
          node.evs.get(0).ev.isInstanceOf[ConvertToTypeTreeAdapt] &&
          node.evs.get(1).ev.isInstanceOf[AdaptDone] ||
          node.evs.length == 1 && node.evs.head.ev.isInstanceOf[AdaptDone] ||
          node.evs.length == 1 && node.evs.head.ev.isInstanceOf[TypeTreeAdapt]
        case _ =>
          false
      }
      // Also with adapt-typeTree
    }

    def pf(fxn: Event =>? Boolean): Unit = apply(Filter pf fxn)
    def apply(filt: Filter): Unit = {
      EV.resetEventsCounter()
      // reset the intermediate structure
      root = createNode(null, null)
      previousLevel = -1
      currentNodes.push((root, -1))
      hook = Hook.indentation((level: Int) => {
        case ev if filt(ev)=> reportWithLevel(ev, level); NoResponse
      })
      hook hooking CompileWrapper.cc(srcs)
    }
  }

  //TODO include settings
  def buildStructure(srcs: List[String], settings: Settings, fxn: Filter, label: String) : (Tree, List[ETreeNode]) = {
    val builder = new EventTreeStructureBuilder(srcs, label)
    builder(fxn)
    (builder.t, builder.initialGoals)
  }
  
  trait VisualPrefuseNode {
    var goal = false // for caching purposes so that we don't have to constantly
                     // check neighbors
  }

  // TODO: this will need to be refactored to separate from the main UI
  case class ETreeNode(val ev: Event, var evs: ListBuffer[ETreeNode], 
    parentENode: Option[ETreeNode], pfuseNode: Node) extends VisualPrefuseNode {
    override def toString =
      if (ev != null) {
        ev match {
          case tpchecker:TyperTyped =>
            val str = tpchecker.expl match {
              case _: TypeExplicitTreeReturnType =>
                "Typecheck explicit return type" // take into account Unit
              case _: TypeDefConstr =>
                "Typecheck constructor's body"
              case _: TypeDefBody =>
                "Typecheck definition's body"

              case _: TypeFunctionApply =>
                "Typecheck function"
              case _: TypeArgsApply =>
                "Typecheck arguments"
              case _: TypeArgApply =>
                "Typecheck argument"
              case _: TypeArgsInOverloadedApply =>
                "Typecheck arguments \n in overloaded function application"
              case _: TypeArgInOverloadedApply =>
                "Typecheck argument without \n expected type \n in overloaded function application"
              case _: TypeTypeConstructorInNew =>
                "Typecheck type constructor for new"
              case _: TypeEtaExpandedTreeWithWildcard =>
                "Typecheck eta-expanded tree\n without expected type"
              case _: TypeEtaExpandedTreeWithPt =>
                "Typecheck eta-expanded tree\n with expected type"
              case _: TypeFunctionTypeApply =>
                "Typecheck function \n in type application"
              case _: TypeHigherKindedTypeApplyWithExpectedKind =>
                "Typecheck higher-kind type with expected kind"
              case _: TypeHigherKindedTypeApplyOverloaded =>
                "Typecheck overloaded polymorphic type"
              case _: TypeHigherKindedTypeForAppliedType =>
                "Typecheck higher-kinded type \n in applied type"
              case _: TypeFunctionParameter =>
                "Typecheck function parameter"
              case _: TypeFunBody =>
                "Typecheck function body"
              case _: TypeArgsStandalone =>
                "Typecheck arguments \n without expected type"
              case _: TypeArgStandalone =>
                "Typecheck argument without \n expected type"
              case _: TypeStatementInBlock =>
                "Typecheck statement"
              case _: TypeExistentialTypeClause =>
                "Typecheck existential type-clause"
              case _: TypeRefinementStatement =>
                "Typecheck refinement statement"
              case _: TypeUseCaseStatement =>
                "Typecheck usecase statement"
              case _: TypePackageStatement => 
                "Typecheck package member"
              case e: TypeTemplateStatement =>
                val parent = e.templateInfo match {
                  case ClassTemplate => "class"
                  case ModuleTemplate => "object"
                  case TraitTemplate => "trait"
                }
                e.stat match {
                  case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) =>
                    "Typecheck " + parent + " constructor"
                  case _ =>
                    "Typecheck " + parent + " member"
                }
              case _: TypeLastStatementInBlock =>
                //"typecheck-last-statement"
                "Typecheck last statement"
              case _: TypeQualifierInSelect =>
                "Typecheck qualifier"
              case _: TypeSuperQualifier =>
                "Typecheck super-dot qualifier"
              case _: TypeQualifierInSuper =>
                "Typecheck qualifier in super-dot"
              case _: TypeValType =>
                "Typecheck value's type"
              case _: TypeTypeConstructor =>
                "Typecheck type constructor"
              case _: TypeInitialSuperType =>
                "Typecheck first parent \n as initial supertype"
              case e: TypeParentMixin =>
                "Typecheck mixin \n" + anyString(e.mixin)
              case _: TypeCurrentTraitSuperTpt =>
                "Typecheck current \nsuper-type"
              case _: TypeFirstConstructor => 
                "Typecheck transformed \n primary constructor" // without 'transformed'
              case etree: TypeClassTypeParameter =>
                "Typecheck class type-parameter"
              case _: TypeHigherOrderTypeParameter =>
                "Typecheck higher-order type-parameter"
              case tp: TypeDefTypeParameter =>
                "Typecheck def \n type-parameter " 
              case tp: TypeDefParameter =>
                if (tp.param.symbol.hasFlag(symtab.Flags.IMPLICIT)) "Typecheck implicit parameter" else "Typecheck parameter"
              case _: TypeExplicitTypeAnnotation =>
                "Typecheck type annotation"
              case _: TypeAnnotatedExpr =>
                "Typecheck type-annotated expression"
              case _: TypeAppliedImplicitView =>
                "Typecheck application of inferred view\n that adapts qualifier"
              case _: TypeAdaptedQualifer =>
                "Typecheck qualifier adapted to member"
              
              // Bounds
              // better explanation for default bounds
              case lb: TypeLowerBound =>
                lb.bound match {
                  case Select(_, tpnme.Nothing) =>
                    "Typecheck lower bound:Nothing"
                  case _ =>
                    "Typecheck lower bound"
                }
                
              case hb: TypeHigherBound =>
                hb.bound match {
                  case Select(_, tpnme.Any) =>
                    "Typecheck higher bound:Any"
                  case _ =>
                    "Typecheck higher bound"
                }

              // Adapt
              case _: TypeQualifierWithApply =>
                "Typecheck qualifier with \n 'apply()' member"
              
              // Namers
              case _: MethodReturnType =>
                "Typecheck return type"
              case _: MethodReturnTypeAgain =>
                "Typecheck return type (again)"
              case _: InferredMethodReturnType =>
                "Typecheck return type inferred from body"
              case e: ValExplicitType =>
                "Typecheck " + (if (e.sym.isMutable) "variable" else "value") + "'s \n type annotation"
              case _: TypeAbstractTpeBounds =>
                "Typecheck bounds"
              case _: TypeValDefBody =>
                "Typecheck value's body"
              case _: TypeMethodDefBody =>
                "Typecheck method's body"
              
              // implicits/infer
              case _: TypeImplicitViewApplication =>
                "Typecheck application of (found)\n" +
                "implicit view"
              case _: FirstTryTypeTreeWithAppliedImplicitArgs =>
                "Typecheck application of\n" +
                "(inferred) implicit arguments"
              case _: FallbackImplicitArgsTypeClean =>
                "Fallback\n Applying implicit arguments failed.\n" +
                "Type and adapt original expression without expected type"
              case _ =>
                ETreeNode.fmt
            }
            (ev formattedString str)
            // + "-" + tpchecker.tree.getClass.toString
          case _ =>
            ev formattedString ETreeNode.fmt
        }
      } else "Typecheck full tree" // root
    def fullInfo =
      if (ev != null)
        ev match {
          case evTyped: TyperTyped =>
            //TODO This still needs adjustment
            val tpe = if (evTyped.tree.tpe != null) evTyped.tree.tpe else if (evTyped.tree.symbol != null) evTyped.tree.symbol.tpe else null
            /*(evTyped formattedString ETreeNode.fmtFull) + "\n" + 
            "Type of tree: [ " + tpe + " ] expected: [ " + evTyped.pt + " ]" + 
            "\nDebugging info: " + evTyped.tree.getClass +
            "\nEvent toString: " + evTyped.eventString*/
            evTyped.expl + "\n\n" +
            "Typechecking tree: \n " +
            evTyped.tree + "\n\n" +
            "\nExpected type: " + (if (evTyped.pt == WildcardType) "None" else anyString(evTyped.pt)) +
            (if (tpe != null) "\nType of tree set to: " + anyString(tpe) else "Tree not yet typed")
            //"\nTree class " + evTyped.tree.getClass + " pos " + evTyped.tree.pos

          case _ => 
            ev formattedString ETreeNode.fmtFull
        }
      else "Typecheck full tree" // root
  }
  object ETreeNode {
    def emptyETreeNode(ev: Event, parentENode: Option[ETreeNode], pfuseNode: Node) =
      ETreeNode(ev, new ListBuffer(), parentENode, pfuseNode)
//    val fmtFull = "[%ph] [%tg] %ev %po %id" // TODO this should be configurable
    //val fmtFull = "[%ph] [%tg] %ev" // TODO this should be configurable
    val fmtFull = "%ev"
    val fmt = "%ln"
  }

  class SwingBrowser {
    def browse(srcs: List[String], settings: Settings) {
      val filtr =  Filter.and(Filter pf {
        case _: TyperTypeSet                => false
        case _: DebugEvent                  => false
        case _: TyperEvent                  => true
// TODO: re-enable        case _: ImplicitEvent               => true
        case _: ImplicitMethodTpeAdaptEvent => true
        case _: InferEvent                  => true
        case _: AdaptToEvent                => true
        case _: DoTypedApplyEvent           => true
        case _: NamerEvent                  => true
        case _: ValidateParentClassEvent    => true
        //case _: TyperDone => true
        case _: AdaptEvent                  => true
        case _: TypingBlockEvent            => true // typecheck block
//        case _: NewContext                  => true
        case _: ErrorEvent           => true
      }, EVDSL.ph <= 4)

      val NODESLABEL = "event.node"
      //val NODESLABEL = "name"
      val (prefuseTree, goals) = buildStructure(srcs, settings, filtr, NODESLABEL)
      //val prefuseTree1 = new TreeMLReader().readGraph("/home/hubert/lib/prefuse-beta/data/chi-ontology.xml.gz").asInstanceOf[Tree]
      val frame = new TypeBrowserFrame(prefuseTree, srcs, NODESLABEL, goals)
      //val frame = new TypeBrowserFrame(prefuseTree1, List(), NODESLABEL)
      val lock = new Lock()
      frame.createFrame(lock)
     
      lock.acquire
    }
  }

}


object TypeDebuggerUI {
  // currently fix the type of events and phases filtered
  // only parse
  def main(args: Array[String]) {
    // parse input: sources, cp, d
    val settings = new Settings()
    settings.Yrangepos.value = true
    settings.stopAfter.value = List("typer")
    
    val command = new CompilerCommand(args.toList, settings)
    val tb = new TypeBrowser {
      val global = new Global(settings, new ConsoleReporter(settings))
    }
    
    val b = new tb.SwingBrowser()

    //println("Command files: " + command.files)
    //println("Settings: " + settings)
    b.browse(command.files, settings)
  }
}
