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
    
    // Do not show all nodes in the UI. When dealing with parent nodes, it has to be ensured
    // that closing events are also filtered out.
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
       case _: InferredImplicitAdapt =>
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
      
      def validEvent(ev: Event) = (!invisible.isDefinedAt(ev) || invisible(ev))
      
      def setSingleNode(parent: UINode[PrefuseEventNode], node: BaseTreeNode[EventNode],
                        t: Tree): UINode[PrefuseEventNode] = {
        val (realParent, pfuseNode) =
          if (parent == null) (None, t.addRoot())
          else (Some(parent), t.addChild(parent.pfuseNode)) 
        val node1 = new PrefuseEventNode(node.ev, realParent, pfuseNode)
        node1.pfuseNode.set(label, node1)
        node1.children ++= mapNodeChildren(node1, node.children.toList, t).flatten
        if (errorNodes0.contains(node)) {
          errorNodes += node1
        }
        node1
      }
      
      def processChildren(parent: UINode[PrefuseEventNode], child: BaseTreeNode[EventNode]): Option[UINode[PrefuseEventNode]] = child.ev.file match {
        case Some(absFile) =>
          def tree = trees(absFile)
          if (parent == null) {
            Some(setSingleNode(parent, child, tree))
          } else if (validEvent(child.ev) && !hideEvent(child))  {
            simplifyIfPossible(setSingleNode(parent, child, tree), tree)
          } else if (child.children.length > 0) {
            // Get single child that is visible
            def bfs[T](node: BaseTreeNode[T]): List[BaseTreeNode[T]] =
              node.children.toList.flatMap(c => if (validEvent(c.ev)) List(c) else bfs(c))
  
            bfs(child) match {
              case single::Nil =>
                simplifyIfPossible(setSingleNode(parent, single, tree), tree)
                
              case err@_::_ =>
                debug("Filtering node that contains more than one valid child event")
                debug("Note: This may cause some unexpected behaviour")
                debug("FILTERING: " + child.ev.getClass)
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
      
      def simplifyIfPossible(top: UINode[PrefuseEventNode], tree: Tree): Option[UINode[PrefuseEventNode]] = {
        if (filterOutStructure(top)) {
          // prune unnecessary node
          tree.removeChild(top.pfuseNode)
          None
        } else Some(top)
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
      
      // Given processed events we can do better analysis of the context
      // and decide whether the subtree should be included at all
      def filterOutStructure(node: UINode[PrefuseEventNode]): Boolean = {
        node.ev match {
          case adapt: AdaptStart if node.children.length == 2 =>
            node.children(0).ev.isInstanceOf[SubTypeCheck] && {
              node.children(1).ev match {
                case subAdapt: SuccessSubTypeAdapt if subAdapt.value2 =:= WildcardType =>
                  true
                case subAdapt: SuccessSubTypeAdapt if adapt.tree.tpe =:= adapt.pt      =>
                  true
                case constAdapt: ConstantTpeAdapt if adapt.pt =:= WildcardType         =>
                  true
                case _ =>
                  false
              }
            }
          case adapt: AdaptStart if node.children.length == 1     =>
            node.children.head.ev match {
              case subAdapt: SuccessSubTypeAdapt if subAdapt.value2 =:= WildcardType  =>
                true
              case subAdapt: SuccessSubTypeAdapt if adapt.tree.tpe =:= adapt.pt       =>
                true
              case _                                                                  =>
                false
            }
          case _: AdaptStart if node.children.isEmpty         =>
            true
          case _: PolyTpeAdapt                                =>
            node.children.length == 2 &&
            node.children(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
            node.children(1).ev.isInstanceOf[AdaptDone]
          case _: TypeTreeAdapt                               =>
            node.children.length == 2 &&
            node.children(0).ev.isInstanceOf[ConvertToTypeTreeAdapt] &&
            node.children(1).ev.isInstanceOf[AdaptDone] ||
            node.children.length == 1 && node.children.head.ev.isInstanceOf[AdaptDone] ||
            node.children.length == 1 && node.children.head.ev.isInstanceOf[TypeTreeAdapt]
          case sub: SubTypeCheck                              =>
            sub.rhs eq WildcardType
          case qFilter: OverloadedSymDoTypedApply             =>
            !node.children.exists(ch => ch.ev match {
              case _: FilteredDoTypedApply => true
              case _                       => false
            })
          case _                                              =>
            false
        }
        // Also with adapt-typeTree
      }
      
      
      // Used for hiding simple, single events
      def hideEvent(node: BaseTreeNode[EventNode]): Boolean = {
        node.ev match {
          case _: AdaptStart                                  =>
            node.children.length == 2 && {node.children(0).ev match {
              case _: ImplicitMethodTpeAdaptEvent => true
              case _: PolyTpeAdapt                => true
              case _: ApplyAdapt                  => true
              case _                              => false
            }}
          case _: SuccessTryTypedApplyTyper                   =>
            true
          case _: InferredMethodInstance                      =>
            true
          case subst: SimpleTreeTypeSubstitution              =>
            subst.tparams.isEmpty && subst.targs.isEmpty
          case filtered: FilteredDoTypedApply                        =>
            node.parent.get.ev match {
              case qFilter: OverloadedSymDoTypedApply     =>
                val alterNumAfter = {
                  val tree1 = treeAt(filtered.tree)(filtered.time)
                  TypeSnapshot.mapOver(SymbolSnapshot.mapOver(tree1.symbol)(filtered.time).tpe)(filtered.time) match {
                    case OverloadedType(_, alts) => alts.length
                    case _                       => 0
                  }
                }
                val alterNumBefore = {
                  TypeSnapshot.mapOver(SymbolSnapshot.mapOver(qFilter.sym)(qFilter.time).tpe)(qFilter.time) match {
                    case OverloadedType(_, alts) => alts.length
                    case _                       => 0 // cannot happen
                  }
                }
                alterNumAfter == alterNumBefore
            }
          case _                                              =>
            false
        }
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