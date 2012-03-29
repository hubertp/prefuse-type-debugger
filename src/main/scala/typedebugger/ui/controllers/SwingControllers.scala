package scala.typedebugger
package ui
package controllers

import prefuse.Visualization
import prefuse.data.{Graph, Table, Node, Tuple, Edge, Tree}
import prefuse.visual.{VisualItem, NodeItem, EdgeItem}
import prefuse.controls.{ControlAdapter, FocusControl, PanControl, WheelZoomControl,
                         ZoomControl, ZoomToFitControl}
import prefuse.data.expression.{AbstractPredicate, Predicate, OrPredicate}

import java.awt.Color
import java.awt.event._
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter
import javax.swing.text.Highlighter
import javax.swing.event.{CaretListener, CaretEvent}

import scala.collection.JavaConversions._
import scala.tools.nsc.io
import scala.tools.nsc.util.{SourceFile, BatchSourceFile}

trait SwingControllers {
  self: internal.CompilerInfo with UIUtils with internal.PrefuseStructure with internal.EventFiltering =>
    
  import global.{Tree => STree, _}
  import EV._
  import UIConfig.{nodesLabel => label}
  
  import PrefusePimping._
  
  class TypeDebuggerController(prefuseComponent: PrefuseComponent, srcs: List[io.AbstractFile])
    extends SwingFrame(prefuseComponent,"Type debugger 0.0.3", settings.advancedDebug.value) {

    var lastAccessed: Option[NodeItem] = None
    
    val highlightContr = new HighlighterAndGeneralInfo()
    val cleanupAction = new CleanupAction()
    
    private def codeHighlighter = sCodeViewer.getHighlighter()
    sCodeViewer.addCaretListener(new SelectionListener())
   
    def initPrefuseListeners() {
      prefuseComponent.addControlListener(highlightContr)
      prefuseComponent.addControlListener(new AddGoal())
      prefuseComponent.addControlListener(new LinkNode())
      prefuseComponent.addControlListener(new FixedNode())
      prefuseComponent.addControlListener(KeyPressAddGoal)
      prefuseComponent.addControlListener(HiddenEvents)
      
      if (srcs.isEmpty)
        println("[Warning] No files specified for debugging.")
      else 
        loadSourceFile(srcs.head) // TODO: remove restriction
    }
    
    private def loadSourceFile(absFile: io.AbstractFile) {
      // at the moment we only ensure that there is only one
      val src = if (absFile.file.exists)
        io.File(absFile.file).slurp
      else "Missing source code"

      sCodeViewer.setText(src)
    }

    
    // specific adapters, actions
    class SelectionListener() extends CaretListener {
      
      private def orderedPos(e: CaretEvent): (Int, Int) = 
        if (e.getDot > e.getMark) (e.getMark, e.getDot)
        else (e.getDot, e.getMark)
      
      def caretUpdate(e: CaretEvent) {
        debug("Caret update: " + e.getDot + " .... " + e.getMark)
        // locate tree
        srcs match {
          case List(oneSource) =>
            val sFile = new BatchSourceFile(oneSource)
            val (start, end) = orderedPos(e)
            val position = global.rangePos(sFile, start, start, end)
            debug("Selection position: " + position)
            val t = global.locate(position)
            prefuseComponent.grabFocus() // fix focus for key navigation
            debug("Overlapping tree: " + t + "\n with pos " + t.pos + " treeOfClass " + t.getClass)
            if (t != EmptyTree) {
              highlightContr.clearHighlight(true)
              val ref = highlightContr.highlight(position, highlightContr.TargetDebuggingHighlighter)
              highlightContr.activeSelection = ref
              targetedCompile(position)
            } else
              debug("invalid selection")

          case _ => // not supported yet

        }
      }
    }
    
    class HighlighterAndGeneralInfo() extends ControlAdapter {
      var activeSelection: AnyRef =  null
      
      override def itemClicked(item: VisualItem, e: MouseEvent) = 
        if (containsDataNode(item) && e.isAltDown())
            fullEventInfo(asDataNode(item).ev)

      override def itemEntered(item: VisualItem, e: MouseEvent) {
        if (containsDataNode(item)) {
          clearHighlight()
          val node = asDataNode(item)
          node.ev match {
            case e:TreeEvent if e.tree.pos.isRange =>
              val prettyTree = asString(e.tree)
              ASTViewer.setText(prettyTree)
              highlight(e.tree.pos, TreeMainHighlighter)
              
            case e: SymEvent if e.sym.pos.isRange =>
              highlight(e.sym.pos, TreeMainHighlighter)
              
            case _ =>
              debug("No precise position for " + node + ". Trying parents.")
              // Fallback try to find the closest parent that has positions set
              def canHighlight(n: UINode[PrefuseEventNode]) = n.ev match {
                case e: TreeEvent if e.tree.pos.isRange => true
                case e: SymEvent if e.sym.pos.isRange => true
                case _ => false
              }
              
              var nodeWithPos: Option[UINode[PrefuseEventNode]] = node.parent
              while (nodeWithPos.isDefined && !canHighlight(nodeWithPos.get)) {
                nodeWithPos = nodeWithPos.get.parent
              }
              
              // TODO refactor
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
              e.references.foreach((ref:Symbol) => if (ref != null) highlight(ref.pos, TreeReferenceHighlighter))
            case e: TreeReferencesEvent =>
              e.references.foreach((ref:STree) => highlight(ref.pos, TreeReferenceHighlighter))
            case _ =>
          }
        }
      }
      
	    override def itemExited(item: VisualItem, e: MouseEvent) {
	      ASTViewer.setText(null)
	      clearHighlight()
	    }
      
      private[SwingControllers] def highlight(pos: Position, colorSelection: DefaultHighlightPainter): AnyRef = 
        if (pos.isRange) codeHighlighter.addHighlight(pos.start, pos.end, colorSelection)
        else null
  
      private[SwingControllers] def clearHighlight(force: Boolean = false) = {
        codeHighlighter.getHighlights foreach (h =>
          if (force || h != activeSelection) codeHighlighter.removeHighlight(h)
        )
      }
     
      object TreeMainHighlighter extends DefaultHighlightPainter(Color.red)
      object TreeReferenceHighlighter extends DefaultHighlightPainter(Color.green)
      object TargetDebuggingHighlighter extends DefaultHighlightPainter(new Color(204, 204, 204, 80))
    }
    
    object HiddenEvents extends ControlAdapter {
      private final val PREFIX = "Filtered debugging: "
      override def itemEntered(item: VisualItem, e: MouseEvent) {
        item match {
          case node: NodeItem =>
            // display information about hidden events (if any)
            val children = node.children_[NodeItem].flatMap { ch =>
              if (!ch.isVisible && asDataNode(ch).advanced)
                Some(FilteringOps.map(asDataNode(ch).ev))
              else None
            }.toList
            if (children.nonEmpty)
              statusBar.setText(PREFIX + children.distinct.mkString(","))
          case _ =>
        }
      }
      
      override def itemExited(item: VisualItem, e: MouseEvent) {
        statusBar.setText("")
      }
    }
    
      
    // Handle action on the node of the graph.
	  // Expand the node that was just clicked. Also cleanup all the intermediate nodes leading to it.
	  class FixedNode extends ControlAdapter {
	    override def itemClicked(item0: VisualItem, e: MouseEvent) {
	      if (!e.isControlDown() || !e.isShiftDown())
	        return
	      
	      item0 match {
	        case item: NodeItem =>
			      val vis = item.getVisualization
			      val fGroup = vis.getFocusGroup(PrefuseComponent.fixedNodes)
			      if (fGroup.containsTuple(item)) {
			        fGroup.removeTuple(item)
			        vis.getFocusGroup(PrefuseComponent.toRemoveNodes).addTuple(asDataNode(item).pfuseNode)
			        cleanupLinkPath(item, vis)
			      } else {
			        fGroup.addTuple(item)
			      }
			      lastAccessed = Some(item)
	        case _ =>   
	      }
	    }
	    
	    def cleanupLinkPath(starting: NodeItem, vis: Visualization) {
	      var n = asDataNode(starting)
	      val tsNonGoal = vis.getFocusGroup(PrefuseComponent.nonGoalNodes)
	      val tsRemove = vis.getFocusGroup(PrefuseComponent.toRemoveNodes)
	      while (!n.goal && n.parent.isDefined) {
	        tsNonGoal.removeTuple(n.pfuseNode)
	        tsRemove.addTuple(n.pfuseNode)
	        n = n.parent.get
	      }
	    }
	  }
	  
	  object KeyPressAddGoal extends ControlAdapter {
	    
	    val validKeys = List(KeyEvent.VK_DOWN, KeyEvent.VK_UP, KeyEvent.VK_LEFT, KeyEvent.VK_RIGHT)
	    def lastNode: Option[NodeItem] =
	      if (lastAccessed.isDefined)
          lastAccessed
        else {
          // Find bottom most event
          val vis = prefuseComponent.getVisualization
          val ts = vis.getFocusGroup(Visualization.FOCUS_ITEMS)
          val goals = vis.getFocusGroup(PrefuseComponent.openGoalNodes)
          ts.tuples().find {
            case item: NodeItem =>
              val item1 = asDataNode(item)
              !item1.parent.isDefined || !goals.containsTuple(item1.parent.get.pfuseNode)
            case _ => false } match {
            case Some(top: NodeItem) =>
              top.children_[NodeItem].find (asDataNode(_).goal) match {
                case Some(child) =>
                  Some(child)
                case _ =>
                  println("[warning] cannot navigate, found bug")
                  // bug
                  None
              }
            case None =>
              //prefuseComponent.treeRoot()
              None
            }
        }

	    override def itemKeyPressed(item: VisualItem, k: KeyEvent) = keyPressed(k)
	    
	    override def keyPressed(k: KeyEvent): Unit = lastNode match {
	      case Some(last) =>
	        debug("Key pressed. Last accessed event " + last)
          val keyCode = k.getKeyCode	
  	      val vis = last.getVisualization
  	      val vGroup = vis.getFocusGroup(Visualization.FOCUS_ITEMS)
  	
  	      prefuseComponent.tooltipController.clearTooltip()
  	      keyCode match {
  	        case KeyEvent.VK_DOWN =>
  	          // expand down (if necessary)
  	          val n = asDataNode(last)
  	          if (n.parent.isDefined)
  	            navigate(n.parent.get.pfuseNode, vis)
  	          
  	        case KeyEvent.VK_LEFT =>
  	          // expand left neighbour (if possible)
  	          val prevSibling = last.getPreviousSibling()
  	          if (prevSibling != null)
  	            navigate(asDataNode(prevSibling).pfuseNode, vis)
  	          
  	        case KeyEvent.VK_RIGHT =>
  	          // expand right neighbour (if possible)
  	          val nextSibling = last.getNextSibling()
  	          if (nextSibling != null)
  	            navigate(asDataNode(nextSibling).pfuseNode, vis)
  	
  	        case KeyEvent.VK_UP =>
  	          if (last.getChildCount() > 0)
  	            navigate(asDataNode(last.getFirstChild()).pfuseNode, vis)
  
  	        case KeyEvent.VK_E  => // expand
  	          expand(last)
  
  	        case KeyEvent.VK_C  => // collapse
  	          collapse(last)
  
  	        case other =>
  	          controlKeyPressed(other, last)

          }

  	    case None =>

	    }
	    
	    def controlKeyPressed(k: Int, node: NodeItem) = k match {
	      case KeyEvent.VK_ENTER =>
	        val pNode = asDataNode(node).pfuseNode
	        node.getVisualization.items(new VisualItemSearchPred(pNode)).toList match {
	          case List(single: VisualItem) =>
	            prefuseComponent.tooltipController.showItemTooltip(single)
	          case _ =>
	        }
	        
	      case KeyEvent.VK_ESCAPE =>
	        prefuseComponent.tooltipController.clearTooltip()
	      
	      case _ =>
	        
	    }
	    
	    def navigate(n: Node, vis: Visualization) {
	      vis.items(new VisualItemSearchPred(n)).toList match {
	        case List(first: NodeItem) =>
	          AddGoal.addClickedItem(first, vis)
	          highlightContr.itemEntered(first, null)
	          // need to schedule the action by hand since keycontrol doesn't do it
	          prefuseComponent.reRenderView()
	        case _ =>
	          println("incorrect search " + n)
	      }
	    }
	    
	    def expand(last: NodeItem) = {
	      val nodes = advancedNodes(!_.isVisible, last) 
	      if (nodes.nonEmpty) {
	        nodes.distinct foreach prefuseComponent.adv.enableOption
	        prefuseComponent.reRenderView()
	      }
	    }
	    
	    def collapse(last: NodeItem) = {
        val nodes = advancedNodes(_.isVisible, last) 
        if (nodes.nonEmpty) {
          nodes.distinct foreach prefuseComponent.adv.disableOption
          prefuseComponent.reRenderView()
        }
      }
	    
	    private def advancedNodes(visibleState: NodeItem => Boolean, last: NodeItem) = {
	      last.children_[NodeItem] flatMap { ch =>
	        if (visibleState(ch) && asDataNode(ch).advanced)
	          Some(FilteringOps.map(asDataNode(ch).ev))
	        else None
	      } toList
	    }
	  }
	  
    // Find node which is somehow linked (tree or symbol reference) to the
	  // one that was just clicked (with Ctrl).
	  // Use case: clicking on a node to see at what point it's type was set.
	  class LinkNode extends ControlAdapter {
	    
	    import PrefuseComponent.{linkGroupNodes => linkGroup,
	                             treeNodes, nonGoalNodes, openGoalNodes}
	    
	    class FindNode(id: Int) extends AbstractPredicate {
	      override def getBoolean(t: Tuple): Boolean = t match {
	        case node: NodeItem if containsDataNode(node) =>
	          val ev = asDataNode(node).ev
	          ev != null && ev.id == id
	        case _ =>
	          false
	      }
	    }
	    
	    def addLinkPath(starting: NodeItem, vis: Visualization) {
	      var n = asDataNode(starting)
	      val tsNonGoal = vis.getFocusGroup(nonGoalNodes)
	      while (!n.goal && n.parent.isDefined) {
	        tsNonGoal.addTuple(n.pfuseNode)
	        n = n.parent.get
	      }
	      
	      if (n.goal) {
	        val tsGoal = vis.getFocusGroup(openGoalNodes)
	        while (!tsGoal.containsTuple(n.pfuseNode)) {
	          tsGoal.addTuple(n.pfuseNode)
	          // better check 
	          n = n.children.find(_.goal).get
	        }
	      }
	    }
	    
	    override def itemClicked(item: VisualItem, e: MouseEvent) {
	      if (!e.isControlDown() || e.isShiftDown())
	        return
	        
	      val vis = item.getVisualization
	      item match {
	        case node: NodeItem =>
     	      val eNode = asDataNode(node)
	          eNode.ev match {
	            case e@IdentTyper(tree0) =>
			          debug("[Link] IdentTyper event " + tree0.symbol)
			          val refId = tree0.symbol.previousHistoryEvent(e.id)
			          if (refId != NoEvent.id) {
			              // Find corresponding event and node in the tree
			              //println("Found info in the history: " + refId)
			              val ts2= vis.items_[NodeItem](treeNodes, new FindNode(refId))
			              val tsTarget = vis.getFocusGroup(linkGroup)
			              // will ts2 return NodeItem or Node
			              ts2.foreach(n => {
			                tsTarget.addTuple(n)
			                // need to find common root with the currently visible tree
			                // go until you find goal
			                addLinkPath(n, vis)               
			              })
			          }
			        case _ =>
			          // Do nothing for the moment
			          // we need to find other cases, where
			          // we might want to link
			      }
	        case _ =>
	      }
	    }
	  }
	  
	  // 'Stick' node that was clicked with Shift & Ctrl.
    // It will be visible even if it is not on a path to a goal(errors etc).
    class AddGoal() extends ControlAdapter {
      import AddGoal._
      override def itemClicked(item0: VisualItem, e: MouseEvent) {
        if (e.isControlDown() || e.isShiftDown())
          return
          
        item0 match {
          case item: NodeItem =>
            addClickedItem(item, item.getVisualization())
          case _ =>
        }
      }
    }
  
	  object AddGoal {
	    def addClickedItem(node: NodeItem, vis: Visualization) {
	      // Add or remove from focus group
	      val vis = node.getVisualization
	      val ts1 = vis.getFocusGroup(PrefuseComponent.openGoalNodes)
	      val ts2 = vis.getFocusGroup(PrefuseComponent.nonGoalNodes)
	      val clicked = vis.getFocusGroup(PrefuseComponent.clickedNode)
	
	      cleanupAction.clean(node)
	      clicked.clear()
	      clicked.addTuple(node)
	
	      // identify parent goal
	      val eNode = asDataNode(node)
	      
	      // is any of its children a goal
	      // it has to be already expanded, so this is valid
	      if (eNode.goal || node.outNeighbors_[NodeItem]().exists(asDataNode(_).goal)) {
	        // we are dealing with a goal or its parent
	        eNode.parent match {
	          case Some(parent) =>
	            // expand its parent
	            parent.goal = true
	            ts1.addTuple(parent.pfuseNode)
	          case None =>
	        }
	      } else {
	        // we are dealing with a non-direct goal
	        // expand its children which are non-goals
	        var eNode0 = eNode
	        // goals are all in, so we are fine 
	        // TODO: is this loop necessary? parent should already be expanded, right?
	        while (!eNode0.goal && eNode0.parent.isDefined) {
	          ts2.addTuple(eNode0.pfuseNode)
	          eNode0 = eNode0.parent.get
	        }
	      }
	      lastAccessed = Some(node)        
	    }
	  }
	  
	  class CleanupAction {
	    import PrefuseComponent._
	    def clean(item: VisualItem) {
	      if (!containsDataNode(item))
	        return
	
	      var eNode = asDataNode(item)
	      val vis = item.getVisualization
	      val List(ts1, ts2, ts3, tsRemove) =
	        List(openGoalNodes, nonGoalNodes, linkGroupNodes, toRemoveNodes).map(vis.getFocusGroup(_))
	      
	      // Remove all the link nodes
	      ts3.foreach(tsRemove.addTuple)
	      ts3.clear()
	      
	      if (eNode.goal) {
	        // Collapse all the subgoals above
	        // Currently disable messages view when dealing
	        // with multiple errors (least spanning tree problem)
	        if (eNode.parent.isDefined) {
	          // cached minimal spanning tree
	          val cached = prefuseComponent.nodesAlwaysVisible
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
	        ts2.foreach(tsRemove.addTuple)
	        ts2.clear()
	      } else {
	        // Remove all the other non-essential non-goals
	        // apart from those leading to this node
	        ts2.foreach(tsRemove.addTuple)
	        ts2.clear()
	        var eNode0 = eNode
	        while(eNode0.parent.isDefined && !ts1.containsTuple(eNode0.parent.get.pfuseNode)) {
	          ts2.addTuple(eNode0.pfuseNode)
	          eNode0 = eNode0.parent.get            
	        }
	      }
	    }
	  }
	  
	  // only debugging information
    private def fullEventInfo(ev: Event) {
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
  }
}