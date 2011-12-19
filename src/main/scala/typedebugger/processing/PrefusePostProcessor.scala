package scala.typedebugger
package processing

import internal.{ CompilerInfo, IStructure, PrefuseStructure }

import prefuse.data.{ Tree, Node }

import scala.collection.mutable
import mutable.{ ListBuffer }

import scala.tools.nsc.symtab

trait PrefusePostProcessors {
  self: CompilerInfo with IStructure with PrefuseStructure =>
    
  import global.{Tree => STree, _}
  import EV._

  object EventNodeProcessor {    
    def processTree(prefuseTree: Tree, root: BaseTreeNode[EventNode], errors: List[BaseTreeNode[EventNode]], label: String): (UINode[PrefuseEventNode], List[UINode[PrefuseEventNode]]) = {
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
  
    private class EventNodeProcessor1(t: Tree, unvisible: PartialFunction[Event, Boolean], errorNodes0: List[BaseTreeNode[EventNode]], label: String) {
      val errorNodes: ListBuffer[UINode[PrefuseEventNode]] = ListBuffer()
      
      def process(root: BaseTreeNode[EventNode]) =
        (processChildren(null, root).get, errorNodes.toList)
      
      def processChildren(parent: UINode[PrefuseEventNode], child: BaseTreeNode[EventNode]): Option[UINode[PrefuseEventNode]] = {
        val filter = child.children.nonEmpty && child.children.last.ev.isInstanceOf[DoneBlock] && filterOutStructure(parent)
        // TODO use filter to abandon useless branches
        if (parent == null) {
          val root = new PrefuseEventNode(child.ev, None, t.addRoot())
          root.pfuseNode.set(label, root)
          root.children ++= child.children.map(processChildren(root, _)).flatten
          Some(root)
        } else if (!unvisible.isDefinedAt(child.ev) || unvisible(child.ev)) {
          //println("process-nonroot-child: " + child + " || " + parent.pfuseNode)
          val child1 = new PrefuseEventNode(child.ev, Some(parent), t.addChild(parent.pfuseNode))
          child1.pfuseNode.set(label, child1)
          child1.children ++= child.children.map(processChildren(child1, _)).flatten
          // mapping of error nodes
          if (errorNodes0.contains(child))
            errorNodes += child1
          Some(child1)
        } else None
      }
      
      def simplify(top: UINode[PrefuseEventNode]) {
        val toRemove = top.children.filter {
          child =>
            if (filterOutStructure(child)) {
              //t.removeChild(child.pfuseNode)
              true
            } else false
        }
        if (toRemove.nonEmpty)
          top.children --= toRemove
          //top.children = top.children.filterNot(toRemove contains)
      }
      
      def filterOutStructure(node: UINode[PrefuseEventNode]): Boolean = {
        node.ev match {
          case _: AdaptStart =>
            node.children.length == 2 &&
            node.children(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
            node.children(1).ev.isInstanceOf[AdaptDone] ||
            node.children.length == 1 && node.children.head.ev.isInstanceOf[AdaptDone]
          case _: PolyTpeAdapt =>
            node.children.length == 2 &&
            node.children(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
            node.children(1).ev.isInstanceOf[AdaptDone]
          case _: TypeTreeAdapt =>
            node.children.length == 2 &&
            node.children(0).ev.isInstanceOf[ConvertToTypeTreeAdapt] &&
            node.children(1).ev.isInstanceOf[AdaptDone] ||
            node.children.length == 1 && node.children.head.ev.isInstanceOf[AdaptDone] ||
            node.children.length == 1 && node.children.head.ev.isInstanceOf[TypeTreeAdapt]
          case _ =>
            false
        }
        // Also with adapt-typeTree
      }
    }
  }
}