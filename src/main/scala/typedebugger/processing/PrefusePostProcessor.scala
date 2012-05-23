package scala.typedebugger
package processing

import internal.{ CompilerInfo, IStructure, PrefuseStructure, SyntheticEvents}

import prefuse.data.Tree

import scala.collection.mutable
import scala.tools.nsc.io

trait PrefusePostProcessors {
  self: CompilerInfo with IStructure with PrefuseStructure with SyntheticEvents =>
    
  import global.{Tree => STree, _}
  import EV._

  object EventNodeProcessor {  
    // side effects on prefuse.data.Tree
    def processTree(prefuseTrees: Map[io.AbstractFile, Tree],
        root: BaseTreeNode[EventNode],
        errors: List[BaseTreeNode[EventNode]], label: String): List[UINode[PrefuseEventNode]] = {
      
      prefuseTrees foreach { case (_, tree) => ensureTreeColumnExists(tree, label) }
      
      val filterNodes:PartialFunction[Event, Boolean] =
        if (settings.fullTypechecking.value) (ev: Event) => ev match {case _ => true} else visibleNodes
      new EventNodeProcessor1(prefuseTrees, filterNodes, errors, label).process(root)
    }
    
    def ensureTreeColumnExists(tree: Tree, label: String) {
      val nodeTable = tree.getNodeTable()
      // Dummy for class, not companion object
      if (nodeTable.getColumn(label) == null)
        nodeTable.addColumn(label, (new PrefuseEventNode(null, null, null)).getClass)
    }
    
    // Do not show all nodes in the UI, not necessary
    // Have to be done or ensure that there is no other node attached to them
    // because they cannot be parents
    lazy val visibleNodes: PartialFunction[Event, Boolean] = {
       case _: TyperTypeSet => 
         false
       case _: TyperTyped1Done => // typing event
         false
       case _: NamerDone => // bug in compiler? NamerDone.type crashes
         false
       case _: ImplicitSearchDone =>
         false
       case _: TyperTypedDone =>
         false
       case _: TyperDone =>
         false
       case _: AdaptDone =>
         false
       case _: MainAdaptDone =>
         false
       case _: InferDone =>
         false
       case _: TypesDone =>
         false
       case _: LubGlbDone =>
         false
       case e: InstantiateTypeParams => // produces too much noise currently if (e.formals.isEmpty) =>
         false
       case _: RecoveryEvent =>
         false // we should be able to avoid this filter
       case _: UnitApplyDone =>
         false
       case _: NamerApplyPhase =>
         false
       case init: SymInitializeTyper if init.sym.isInitialized =>
         false
         
       case alts: ImprovesAlternativesCheck if alts.alt1 == NoSymbol || alts.alt2 == NoSymbol =>
         false
    }
  
    // don't reuse the class, since it contains internal result of transforming goal nodes
    private class EventNodeProcessor1(trees: io.AbstractFile => Tree,
      invisible: PartialFunction[Event, Boolean],
      errorNodes0: List[BaseTreeNode[EventNode]], label: String) {
      
      val errorNodes: mutable.ListBuffer[UINode[PrefuseEventNode]] = mutable.ListBuffer()
      
      def process(root: BaseTreeNode[EventNode]) = {
        processUnits(root)
        errorNodes.toList
      }
      
      def processUnits(node: BaseTreeNode[EventNode]) {
        node.ev match {
          case _: TyperApplyPhase =>
            debug("Process unit node: " + node.ev.file)
            processChildren(null, node)
          case _ => node.children foreach processUnits
        }
      }
      
      //val filter = child.children.nonEmpty && child.children.last.ev.isInstanceOf[DoneBlock] && filterOutStructure(parent)
      def validEvent(ev: Event) = (!invisible.isDefinedAt(ev) || invisible(ev))
      def tree(file: io.AbstractFile): Tree = trees(file)
      
      def processChildren(parent: UINode[PrefuseEventNode], child: BaseTreeNode[EventNode]): Option[UINode[PrefuseEventNode]] = child.ev.file match {
        case Some(absFile) =>
          if (parent == null) {
            // get respective unit and tree from child
            val root = new PrefuseEventNode(child.ev, None, tree(absFile).addRoot())
            root.pfuseNode.set(label, root)
            root.children ++= child.children.map(processChildren(root, _)).flatten
            Some(root)
          } else if (validEvent(child.ev))  {
            val child1 = new PrefuseEventNode(child.ev, Some(parent), tree(absFile).addChild(parent.pfuseNode))
            child1.pfuseNode.set(label, child1)
            // mapping of error/goal nodes
            if (errorNodes0.contains(child)) {
              errorNodes += child1
            }
            child1.children ++= mapNodeChildren(child1, child.children.toList, tree(absFile)).flatten
            Some(child1)
          } else if (child.children.length > 0) {
            // Get single child that is visible
            def bfs[T](node: BaseTreeNode[T]): List[BaseTreeNode[T]] =
              node.children.toList.flatMap(c => if (validEvent(c.ev)) List(c) else bfs(c))
  
            bfs(child) match {
              case single::Nil =>
                val child1 = new PrefuseEventNode(single.ev, Some(parent), tree(absFile).addChild(parent.pfuseNode))
                child1.pfuseNode.set(label, child1)
                child1.children ++= single.children.map(processChildren(child1, _)).flatten
                // mapping of error nodes
                if (errorNodes0.contains(single)) {
                  errorNodes += child1
                }
                Some(child1)
                
              case err@_::_ =>
                println("Filtering node that contains more than one valid child event")
                println("Note: This may cause some unexpected behaviour")
                val skipped = err.map(processChildren(parent, _)).flatten
                parent.children ++= skipped
                None
  
              case Nil =>
                None
                
            }
          } else None
        case None =>
            None
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
      // this assumes that AllEligibleImplicits contain CategorizeImplicits and InfoEligibleTest only
      def mapNodeChildren(node: UINode[PrefuseEventNode], children: List[BaseTreeNode[EventNode]], root: Tree) = node.ev match {
        case _: AllEligibleImplicits =>
          groupEligibleEvents(node, children, root)
        case _                       => // default
          children.map(processChildren(node, _))
      }
      
      def groupEligibleEvents(parent: UINode[PrefuseEventNode], children: List[BaseTreeNode[EventNode]], root: Tree) = {
        val groupedChildren = children.foldRight(List(List[BaseTreeNode[EventNode]]()))((curr, acc) =>
          if (curr.ev == EV.CategorizeImplicits) List()::acc else (curr :: (acc.head))::acc.tail
        )
        groupedChildren.map { group =>
          group match {
            case h::_   =>
              h.ev match {
                case first: InfoEligibleTest =>
                  val group0Node = new PrefuseEventNode(new GroupEligibleImplicits(first.info.sourceInfo), Some(parent), root.addChild(parent.pfuseNode))
                  group0Node.pfuseNode.set(label, group0Node)
                  group0Node.children ++= group.map(processChildren(group0Node, _)).flatten
                  Some(group0Node)
                case _ =>
                  None
              }
            case Nil    =>
              None
          }
        }
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
      
      def splitAt(ev: Event, list: List[BaseTreeNode[EventNode]]): List[List[BaseTreeNode[EventNode]]] = {
        // the first will be empty thanks to foldRight
        list.foldRight(List(List[BaseTreeNode[EventNode]]()))((curr, acc) =>
          if (curr.ev == ev) List()::acc else (curr :: (acc.head))::acc.tail
        )
      }
    }
  }
}