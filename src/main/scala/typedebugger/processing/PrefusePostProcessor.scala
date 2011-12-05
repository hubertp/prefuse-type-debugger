package scala.typedebugger
package processing

import internal.IStructure

import prefuse.data.{ Tree, Node }

import scala.collection.mutable
import mutable.{ ListBuffer }

import scala.tools.nsc.symtab

trait PrefusePostProcessors {
  self: CompilerInfo with IStructure with StringOps =>
    
  import global.{Tree => STree, _}
  import EV._
  
  class PrefuseEventNode(val underlying: EventNodeProcessor.ENode, val parent: Option[PrefuseEventNode], var pfuseNode: Node) {
    def ev = underlying.ev
    var evs = ListBuffer[PrefuseEventNode]()
    var goal = false // for caching purposes so that we don't have to constantly
                     // check neighbors
    
    // TODO refactor to a separate place that handles all the UI stuff
    override def toString =
      if (ev != null) {
        ev match {
          case tpchecker:TyperTyped =>
            Explanations(tpchecker)
          case _ =>
            Events(ev)
        }
      } else "Typecheck full tree" // root
        
    def fullInfo =
      if (ev != null)
        ev match {
          case evTyped: TyperTyped =>
            //TODO This still needs adjustment
            val tpe = if (evTyped.tree.tpe != null) evTyped.tree.tpe else if (evTyped.tree.symbol != null) evTyped.tree.symbol.tpe else null
            /*(evTyped formattedString PrefuseEventNode.fmtFull) + "\n" + 
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
            ev formattedString PrefuseEventNode.fmtFull
        }
      else "Typecheck full tree" // root
  }
  
  object PrefuseEventNode {
    //val fmtFull = "[%ph] [%tg] %ev %po %id" // TODO this should be configurable
    //val fmtFull = "[%ph] [%tg] %ev" // TODO this should be configurable
    val fmtFull = "%ev"
    val fmt = "%ln"
  }

  object EventNodeProcessor {
    type PNode = PrefuseEventNode
    type ENode = EventNode
    
    def processTree(prefuseTree: Tree, root: ENode, errors: List[ENode], label: String): (PNode, List[PNode]) = {
      val nodes = prefuseTree.getNodeTable()
  
      // Dummy for class, not companion object
      nodes.addColumn(label, (new PrefuseEventNode(null, null, null)).getClass)
      //new EventNodeProcessor1(prefuseTree, (ev: Event) => ev match {case _ => true}, errors, label).process(root)
      new EventNodeProcessor1(prefuseTree, visibleNodes, errors, label).process(root)
    }
    
    // partial function => list?
    // Do not show all nodes in the UI, not necessary
    // Have to be done or ensure that there is no other node attached to them
    // because they cannot be parents
    def visibleNodes: PartialFunction[Event, Boolean] = {
       case _: TyperTypeSet => 
         false
       case _: TyperTyped1Done => // typing event
         false
       case _: NamerDone => // bug in compiler? _:NamerDone.type crashes
         false
  // TODO re-enable
  //          case _: ImplicitSearchDone =>
  //            false
       case _: TyperTypedDone =>
         false
       case _: TyperDone =>
         false
       case _: RecoveryEvents =>
         false
       case _: AdaptDone =>
         false
    }
  
    private class EventNodeProcessor1(t: Tree, unvisible: PartialFunction[Event, Boolean], errorNodes0: List[ENode], label: String) {
      val errorNodes: ListBuffer[PNode] = ListBuffer()
      
      def process(root: ENode) =
        (processChildren(null, root).get, errorNodes.toList)
      
      def processChildren(parent: PNode, child: ENode): Option[PNode] = {
        val filter = child.evs.nonEmpty && child.evs.last.ev.isInstanceOf[DoneBlock] && filterOutStructure(parent)
        // TODO use filter to abandon useless branches
        if (parent == null) {
          val root = new PrefuseEventNode(child, None, t.addRoot())
          root.pfuseNode.set(label, root)
          root.evs = child.evs.map(processChildren(root, _)).flatten
          Some(root)
        } else if (!unvisible.isDefinedAt(child.ev) || unvisible(child.ev)) {
          //println("process-nonroot-child: " + child + " || " + parent.pfuseNode)
          val child1 = new PrefuseEventNode(child, Some(parent), t.addChild(parent.pfuseNode))
          child1.pfuseNode.set(label, child1)
          child1.evs = child.evs.map(processChildren(child1, _)).flatten
          // mapping of error nodes
          if (errorNodes0.contains(child))
            errorNodes += child1
          Some(child1)
        } else None
      }
      
      def simplify(top: PNode) {
        val toLeave = top.evs.filter {
          child =>
            if (filterOutStructure(child)) {
              //t.removeChild(child.pfuseNode)
              false
            } else true
        }
        if (top.evs.length != toLeave.length)
          top.evs = toLeave
      }
      
      def filterOutStructure(node0: PNode): Boolean = {
        val node = node0.underlying
        node.ev match {
          case _: AdaptStart =>
            node.evs.length == 2 &&
            node.evs(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
            node.evs(1).ev.isInstanceOf[AdaptDone] ||
            node.evs.length == 1 && node.evs.head.ev.isInstanceOf[AdaptDone] 
          case _: PolyTpeAdapt =>
            node.evs.length == 2 &&
            node.evs(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
            node.evs(1).ev.isInstanceOf[AdaptDone]
          case _: TypeTreeAdapt =>
            node.evs.length == 2 &&
            node.evs(0).ev.isInstanceOf[ConvertToTypeTreeAdapt] &&
            node.evs(1).ev.isInstanceOf[AdaptDone] ||
            node.evs.length == 1 && node.evs.head.ev.isInstanceOf[AdaptDone] ||
            node.evs.length == 1 && node.evs.head.ev.isInstanceOf[TypeTreeAdapt]
          case _ =>
            false
        }
        // Also with adapt-typeTree
      }
    }
  }
}