package scala.typedebugger
package processing

import internal.{ CompilerInfo, IStructure, PrefuseStructure, SyntheticEvents}

import prefuse.data.Tree

import scala.collection.mutable
import scala.tools.nsc.io

trait PrefusePostProcessors extends util.DebuggerUtils {
  self: CompilerInfo with IStructure with PrefuseStructure with SyntheticEvents =>
    
  import global.{Tree => STree, _}
  import EV._

  object EventNodeProcessor {
    
    object FilterAction extends Enumeration {
      val Remove, Replace, NoOp = Value
    }
    
    import FilterAction._
    
    // side effects on prefuse.data.Tree
    def processTree(prefuseTrees: Map[io.AbstractFile, Tree],
        root: BaseTreeNode[EventNode],
        errors: List[BaseTreeNode[EventNode]], label: String): List[UINode[PrefuseEventNode]] = {
      
      prefuseTrees foreach { case (_, tree) => ensureTreeColumnExists(tree, label) }
      
      val filterNodes:PartialFunction[Event, Boolean] =
        if (settings.fullTypechecking.value) (ev: Event) => ev match {case _ => true} else visibleNodes
      new EventNodeProcessor1(prefuseTrees, filterNodes, errors.map(_.ev), label).process(root)
    }
    
    def ensureTreeColumnExists(tree: Tree, label: String) {
      val nodeTable = tree.getNodeTable()
      // Dummy for class, not companion object
      if (nodeTable.getColumn(label) == null)
        nodeTable.addColumn(label, (new PrefuseEventNode(null, null)).getClass)
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
       case init: SymInitializeTyper => //if init.sym.isInitialized =>
         false
         
       case alts: ImprovesAlternativesCheck if alts.alt1 == NoSymbol || alts.alt2 == NoSymbol =>
         false
    }
  
    // don't reuse the class, since it contains internal result of transforming goal nodes
    private class EventNodeProcessor1(trees: io.AbstractFile => Tree,
      invisible: PartialFunction[Event, Boolean],
      errorNodes0: List[Event], label: String) {
      
      val errorNodes: mutable.ListBuffer[UINode[PrefuseEventNode]] = mutable.ListBuffer()
      
      def process(root: BaseTreeNode[EventNode]) = {
        processUnits(root)
        errorNodes.toList
      }
      
      def processUnits(node: BaseTreeNode[EventNode]) {
        node.ev match {
          case _: TyperApplyPhase =>
            debug("Process unit node: " + node.ev.file)
            processChild(null, node) match {
              case Some(root) =>
                root.ev.file match {
                  case Some(absFile) =>
                    updatePrefuseNodeRefs(None, root, trees(absFile))
                  case None          =>
                    throw new Exception("Unit is not associated with a source file")
                }
              case None       =>
                throw new Exception("Fatala error occurred")
            }
            
          case _ => node.children foreach processUnits
        }
      }
      
      def updatePrefuseNodeRefs(parent: Option[UINode[PrefuseEventNode]], node: UINode[PrefuseEventNode], t: Tree): UINode[PrefuseEventNode] = {
        parent match {
          case None      => 
            node.updatePfuseNode(t.addRoot())
            node.pfuseNode.set(label, node)
          case Some(par) =>
            node.updatePfuseNode(t.addChild(par.pfuseNode))
            node.pfuseNode.set(label, node)
        }
        if (errorNodes0.contains(node.ev))
          errorNodes += node
        node.children.foreach(ch => updatePrefuseNodeRefs(Some(node), ch, t))
        node
      }
      
      def validEvent(ev: Event) = (!invisible.isDefinedAt(ev) || invisible(ev))
      def insertToPrefuseModel(node: UINode[PrefuseEventNode]) = node.pfuseNode.set(label, node) 
      
      def setSingleNode(parent: UINode[PrefuseEventNode],
                        node: BaseTreeNode[EventNode]): UINode[PrefuseEventNode] = {
        val realParent = if (parent == null) None else Some(parent)
        val node1 = new PrefuseEventNode(node.ev, realParent)
        node1.children ++= mapNodeChildren(node1, node.children.toList).flatten
        node1
      }
      
      def processChild(parent: UINode[PrefuseEventNode], child: BaseTreeNode[EventNode]): Option[UINode[PrefuseEventNode]] = child.ev.file match {
        case Some(absFile) =>
          if (parent == null) {
            val node = setSingleNode(parent, child)
            Some(node)
          } else if (validEvent(child.ev) && !hideEvent(child))  {
            simplifyIfPossible(setSingleNode(parent, child), parent)
          } else if (child.children.length > 0) {
            // Get single child that is visible
            def bfs[T](node: BaseTreeNode[T]): List[BaseTreeNode[T]] =
              node.children.toList.flatMap(c => if (validEvent(c.ev)) List(c) else bfs(c))
  
            bfs(child) match {
              case single::Nil =>
                simplifyIfPossible(setSingleNode(parent, single), parent)
                
              case err@_::_ =>
                debug("Filtering node that contains more than one valid child event")
                debug("Note: This may cause some unexpected behaviour")
                debug("FILTERING: " + child.ev.getClass)
                val skipped = err.flatMap(processChild(parent, _))
                parent.children ++= skipped
                None
  
              case Nil =>
                None
                
            }
          } else None
          
        case None =>
            None
      }
      
      def simplifyIfPossible(top: UINode[PrefuseEventNode], realParent: UINode[PrefuseEventNode]): Option[UINode[PrefuseEventNode]] = {
        filterOutStructure(top) match {
          case NoOp    =>
            Some(top)
          case Remove  =>
            None
          case Replace =>
            // just add the children to previous parent
            realParent.children ++= top.children
            None
        }
      }


      def mapNodeChildren(node: UINode[PrefuseEventNode], children: List[BaseTreeNode[EventNode]]) = node.ev match {
        case _: AllEligibleImplicits =>
          groupEligibleEvents(node, children)
        case _: InferMethodInstance  =>
          groupCheckBounds(node, children, new MethTypeArgsResTpeCompatibleWithPt())
        case _: InferExprInstance    =>
          groupCheckBounds(node, children, new ExprTypeTpeCompatibleWithPt())
        case _                       => // default
          children.map(processChild(node, _))
      }
      
      def groupCheckBounds(parent: UINode[PrefuseEventNode], children: List[BaseTreeNode[EventNode]], initialTpeCompareEvent: => SyntheticEvent) = {
        val checkBoundsFilter: Event => Int = (e: Event) =>
          e match {
            case CompareTypes(_, _, _, _)    => 0
            case SolveSingleTVar(_, _, _)    => 1
            case InstantiateTypeVars(_, _)   => 1
            case IsWithinBounds(_, _)        => 2
            case IsTArgWithinBounds(_, _, _) => 3
            case _ => 4
        }
        val subgroupNodes = List(initialTpeCompareEvent, new TryToSolveTVars(), new GroupCheckConstrInstantiationsBounds(), new GroupCheckBoundsOfTArgs())
        val grouped  = children.groupBy(ch => checkBoundsFilter(ch.ev))
        
        val rest1 = grouped(4).map(processChild(parent, _))
        def groupEventsTogether(syntheticEvent: SyntheticEvent, kind: Int, evs: List[BaseTreeNode[EventNode]]) = {
          evs match {
            case Nil => (None, 0)
            case l0 =>
              val idx = children.prefixLength(ch => checkBoundsFilter(ch.ev) != kind)
              val groupNode = new PrefuseEventNode(syntheticEvent, Some(parent))
              groupNode.children ++= l0.flatMap(processChild(groupNode, _))
              (Some(groupNode), idx)
          }
        }

        val subgroups = (subgroupNodes.zipWithIndex).map(group => groupEventsTogether(group._1, group._2, grouped.getOrElse(group._2, Nil)))
        (rest1.zipWithIndex).foldLeft(List[Option[UINode[PrefuseEventNode]]]()){(acc, current) => {
          val currentIdx = current._2
          val toAppend = subgroups.filter(_._2 == currentIdx).map(_._1) // todo: avoid parsing the subgroups all the time
          acc ++ toAppend ++ List(current._1)
        }}
      }

      // this assumes that AllEligibleImplicits contain CategorizeImplicits and InfoEligibleTest only
      def groupEligibleEvents(parent: UINode[PrefuseEventNode], children: List[BaseTreeNode[EventNode]]) = {
        val groupedChildren = children.foldRight(List(List[BaseTreeNode[EventNode]]()))((curr, acc) =>
          if (curr.ev == EV.CategorizeImplicits) List()::acc else (curr :: (acc.head))::acc.tail
        )
        groupedChildren.map { group =>
          group match {
            case h::_   =>
              h.ev match {
                case first: InfoEligibleTest =>
                  val group0Node = new PrefuseEventNode(new GroupEligibleImplicits(first.info.sourceInfo), Some(parent))//, root.addChild(parent.pfuseNode))
                  group0Node.children ++= group.flatMap(processChild(group0Node, _))
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
      // At this point we can also provide more context to any nodes and update it with more info.
      def filterOutStructure(node: UINode[PrefuseEventNode]): FilterAction.Value = {
        if (settings.noFiltering.value) NoOp
        else {var helper = NoOp; node.ev match {
          case AdaptStart(tree, _) if (tree.tpe eq ErrorType) || (tree.tpe eq NoType) =>
            Remove
          case AdaptStart(tree, pt) if node.children.length == 2 =>
            node.children(0).ev.isInstanceOf[SubTypeCheck] && {
              node.children(1).ev match {
                case SuccessSubTypeAdapt(_, _, tpe2) if tpe2 eq WildcardType                  =>
                  { helper = Remove; true }
                case SuccessSubTypeAdapt(_, _, _) if !(pt eq WildcardType) && tree.tpe =:= pt =>
                  { helper = Remove; true }
                case ConstantTpeAdapt(_, _) if pt eq WildcardType                             =>
                  { helper = Remove; true }
                case _                                                                        =>
                  false
              }
            }
            helper
          case AdaptStart(tree, pt) =>
            node.children match {
              case Seq(single) =>
                single.ev match {
                  case SuccessSubTypeAdapt(_, _, tpe2) if tpe2 eq WildcardType                =>
                    Remove
                  case SuccessSubTypeAdapt(_, _, _) if !(pt eq WildcardType) && tree.tpe =:= pt       =>
                    Remove
                  case NotASubtypeAdapt(_, _)                                                 =>
                    Replace
                  case _                                                                      =>
                    NoOp
                }
              case Seq()       =>
                Remove
              case _           =>
                NoOp
            }
          case _: PolyTpeAdapt                                =>
            (node.children.length == 2 &&
            node.children(0).ev.isInstanceOf[SuccessSubTypeAdapt] &&
            node.children(1).ev.isInstanceOf[AdaptDone] && {helper = Remove; true})
            helper
          case _: TypeTreeAdapt                               =>
            (node.children.length == 2 &&
            node.children(0).ev.isInstanceOf[ConvertToTypeTreeAdapt] &&
            node.children(1).ev.isInstanceOf[AdaptDone] ||
            node.children.length == 1 && node.children.head.ev.isInstanceOf[AdaptDone] ||
            node.children.length == 1 && node.children.head.ev.isInstanceOf[TypeTreeAdapt]) &&
            {helper = Remove; true}
            helper
          case SubTypeCheck(_, rhsTpe) if rhsTpe eq WildcardType                            =>
            Remove
          case _: SubTypeCheckRes                                                           =>
            filterOutStructure(node.parent.get) match {
              case Replace => Remove
              case _       => NoOp
            } 
          case SubTypeCheck(tp1: NullaryMethodType, tp2: NullaryMethodType)                 =>
            Replace
          case _: TyperTyped if node.children.isEmpty                                       =>
            Remove
          case typechecked: TyperTyped =>
            typechecked.expl match {
              case TypePackageQualifier(refTree) if refTree.name == nme.EMPTY_PACKAGE_NAME =>
                Remove
              case _                          =>
                NoOp
            }
          case DoTypedApplyDone(_, tree) =>
            if (tree.isErroneous || tree.isErroneousApply) Remove
            else NoOp
          case _: SelectTreeTyper =>
            node.children.map(_.ev) match {
              case Seq((memSel: SelectTyper)) =>
                Replace
              case _                          =>
                NoOp
            }
          case _: OverloadedSymDoTypedApply             =>
            (!node.children.exists(ch => ch.ev match {
              case _: FilteredDoTypedApply => true
              case _                       => false
            })) && {helper = Remove; true}
            helper
          /*case InstantiateGlbOrLub(tvar, up, _)               =>
            def tvar1 = TypeSnapshot.mapOver(tvar)
            val bounds = if (up) tvar1.constr.hiBounds else tvar1.constr.loBounds
            if (bounds.length < 2) Replace else NoOp*/
          case InstantiateTypeVars(tvars, _) if tvars.length > 1                            =>
            Remove
          case CheckedTypesCompatibility(_, _, fast, res)     =>
            if (fast && !res) NoOp
            else // if types are eq still display the node, for information purposes
              if (!fast && res && !node.parent.get.children.exists( ch => ch.ev match { case _: SubTypeCheck => true; case _ => false }))
                NoOp
              else Remove
          case _                                              =>
            NoOp
        }}
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