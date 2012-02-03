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
      val filterNodes:PartialFunction[Event, Boolean] =
          if (settings.fullTypechecking.value) (ev: Event) => ev match {case _ => true} else visibleNodes
      new EventNodeProcessor1(prefuseTree, filterNodes, errors, label).process(root)
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
       case _: AdaptDone =>
         false
       case _: InferDone =>
         false
       case _: LubGlbDone =>
         false
       case _: RecoveryEvent =>
         false // we should be able to avoid this filter
    }
  
    private class EventNodeProcessor1(t: Tree, unvisible: PartialFunction[Event, Boolean], errorNodes0: List[BaseTreeNode[EventNode]], label: String) {
      val errorNodes: ListBuffer[UINode[PrefuseEventNode]] = ListBuffer()
      
      def process(root: BaseTreeNode[EventNode]) =
        (processChildren(null, root).get, errorNodes.toList)
      
      def processChildren(parent: UINode[PrefuseEventNode], child: BaseTreeNode[EventNode]): Option[UINode[PrefuseEventNode]] = {
        val filter = child.children.nonEmpty && child.children.last.ev.isInstanceOf[DoneBlock] && filterOutStructure(parent)
        def validEvent(ev: Event) = (!unvisible.isDefinedAt(ev) || unvisible(ev))
        // TODO use filter to abandon useless branches
        if (parent == null) {
          val root = new PrefuseEventNode(child.ev, None, t.addRoot())
          root.pfuseNode.set(label, root)
          root.children ++= child.children.map(processChildren(root, _)).flatten
          Some(root)
        } else if (advancedFilter(child)) {
          // Ignore those considered to be more advanced/hidden
          None
        } else if (validEvent(child.ev))  {
          //println("process-nonroot-child: " + child + " || " + parent.pfuseNode)
          val child1 = new PrefuseEventNode(child.ev, Some(parent), t.addChild(parent.pfuseNode))
          child1.pfuseNode.set(label, child1)
          child1.children ++= child.children.map(processChildren(child1, _)).flatten
          // mapping of error nodes
          if (errorNodes0.contains(child)) {
            errorNodes += child1
          }
          Some(child1)
        } else if (child.children.length > 0) {
          // Get single child that is visible
          def breadthFirstSearch[T](node: BaseTreeNode[T]): List[BaseTreeNode[T]] =
            node.children.toList.flatMap(c => if (validEvent(c.ev)) List(c) else breadthFirstSearch(c))

          breadthFirstSearch(child) match {
            case single::Nil =>
              val child1 = new PrefuseEventNode(single.ev, Some(parent), t.addChild(parent.pfuseNode))
              child1.pfuseNode.set(label, child1)
              child1.children ++= single.children.map(processChildren(child1, _)).flatten
              // mapping of error nodes
              if (errorNodes0.contains(single)) {
                errorNodes += child1
              }
              Some(child1)
              
            case err@_::_ =>
              // invalid structure generated, unable to recover?
              println("Filtering node that contains more than one valid child event")
              println("Note: This may cause some unexpected behaviour")
              //println(err.map(_.ev.getClass).mkString("[", ",", "]"))
              //println("In node: " + child.ev.getClass)
              val skipped = err.map(processChildren(parent, _)).flatten
              parent.children ++= skipped // TODO
              None

            case Nil =>
              None
              
          }
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
      
      
      // Should split into proper advanced and synthetics
      def advancedFilter(node: BaseTreeNode[_]): Boolean =
        !settings.advancedDebug.value && (node.ev match {
          case _: ConvertConstrBody =>
            true

          case e: ValidateParentClass =>
            e.parent.symbol != null && (e.parent.symbol == definitions.ScalaObjectClass ||
                                        e.parent.symbol == definitions.ObjectClass)

          case e: TyperTyped =>
            val res = e.tree match {
              // Should make it more specific, i.e. search for synthetics 
              case ddef: DefDef if ddef.name == nme.CONSTRUCTOR || (ddef.symbol != null && ddef.symbol.isSynthetic) =>
                true
              case _ =>
                false
            }
            if (!res) {
              e.expl match {
                case e@TypeTemplateStatement(stat) =>
                  if (stat.symbol != null) {
                    stat.symbol.isSynthetic || stat.symbol.isGetter
                  } else false
                case _ =>
                  false
              }
            } else true
            
          case e: ProtoTypeArgsDoTypedApply =>
            true
            
          case e: ImplicitsEligibility =>
            true

          case e: CheckTypesCompatibility =>
            true
            
          case _ =>
            false

        })
    }
  }
}