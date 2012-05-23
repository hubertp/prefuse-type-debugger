package scala.typedebugger
package internal

import scala.tools.nsc.io
import scala.collection.mutable.{ ListBuffer, Stack }

trait StructureBuilders {
  self: CompilerInfo with IStructure with CompilationTools with SyntheticEvents =>

  // Wrapper around the compiler that logs all the events
  // and creates the necessary structure (independent of UI)
  class InstrumentedRun(nodesLabel: String, filt: global.EV.Filter) extends CompilerRunWithEventInfo {
    import global.{EV, Position, NoPosition, treeAt}
    import EV._
    
    private[this] var _root: BaseTreeNode[EventNode] = _
    private[this] var _errorNodes: List[BaseTreeNode[EventNode]] = Nil

    // List of nodes that are currently open
    private val currentNodes = new Stack[(BaseTreeNode[EventNode], Int)]()
    var previousLevel: Int = -1 // Start at root
    
    // Hook that enables us to collect all the events
    private var hook: Hook.IndentationHook = _
    
    
    private def testErrorPos(ev: Event): Boolean = {
      def extractPos(ev0: Event): Position = ev0 match {
        case e0: HardErrorEvent   => e0.errPos
        case e0: ContextTypeError => e0.errPos
        case _                    => NoPosition
      }
      val evPos = extractPos(ev).focus
      evPos == NoPosition || _errorNodes.forall (node => extractPos(node.ev).focus != evPos)
    }
    
    private def createNode(ev: Event, parentENode: BaseTreeNode[EventNode])
                           (implicit statPos: Position): BaseTreeNode[EventNode] = {
      val evNode = new EventNode(ev, new ListBuffer(), if (parentENode == null) None else Some(parentENode))
      // We want them in the order of appearance
      val addToGoals = ev match {
        case e: HardErrorEvent =>
          testErrorPos(ev)
        case e: ContextTypeError if (e.errType == ErrorLevel.Hard) =>
          testErrorPos(ev)
        case e: TyperTyped if statPos != NoPosition && e.tree.pos.sameRange(statPos) =>
          true
          //e.expl match {
          //  case expl: StatementExplanation if expl.stat.pos.sameRange(statPos) =>
          //    errorNodes = evNode::errorNodes
          //  case _ =>
          //}
        case _ =>
          false
      }
      if (addToGoals) _errorNodes = evNode::_errorNodes
      evNode
    }
   
    // analyze the logged events and build necessary structure for the tree
    def reportWithLevel(ev: Event, level: Int)(implicit statPos: Position = NoPosition) {
      implicit def nodeWithLevel(a: BaseTreeNode[EventNode]): (BaseTreeNode[EventNode], Int) = (a, level)
      implicit def onlyNode(a: Tuple2[BaseTreeNode[EventNode], Int]) = a._1

      ev match {
        case _ if previousLevel < level =>
          // Event that is already in an enclosed block
          // This relies on the fact that blocks are correctly opened/closed
          // and events that are opening blocks are not filtered (otherwise we can get some inconsistency)
          // RecoveryEvent to some extent helps in this case, when we are dealing with exceptions.
          val top = currentNodes.top
          if (top.children.isEmpty)
            top.children += createNode(ev, top)
          else {
            val last = top.children.last
            ev match {
              case _: DoneBlock =>
                // Don't push if it is at the same time a done block
              case _            =>
                currentNodes.push(last)
            }
            last.children += createNode(ev, last)
          }
          previousLevel = level
  
        case rec: ExceptionRecoveryEvent =>
          // we don't an example where an exception crashes our instrumentation
          // possibly some cyclic reference problem
          throw new Exception("Bug in Type Debugger! Report the file")
          assert(previousLevel == level, "recovering from an exception")
          // Exception occured, need to backtrack to some previous opening
          while (currentNodes.top._1.ev != rec.lastOpen)
            currentNodes.pop()
            
          val baseLevel = currentNodes.top._2
          previousLevel = baseLevel
          hook.resetIndentation(baseLevel)
          
        case done: DoneBlock  =>
          assert(currentNodes.nonEmpty,
                  "stack of current nodes cannot be empty on end of the block for " + ev + " " + ev.getClass)
          val top = currentNodes.pop()
          // update event for TyperTyped to point to the correct resulting tree
          done match {
            case TyperTypedDone(resTree1, _) =>
              top.ev match {
                case dest: TyperTyped =>
                  dest.resTree = (resTree1, done.time)
                case _ =>
              }
            case MainAdaptDone(_, resTree1) =>
              top.ev match {
                case dest: AdaptStart =>
                  dest.resTree = (resTree1, done.time)
                case _ =>
              }
            case _ =>
          }
          
          top.children += createNode(ev, top)
        
        case _             =>
          previousLevel = level
          assert(!currentNodes.isEmpty)
          val top = currentNodes.top
          top.children += createNode(ev, top)
      }
    }
  
    def run(srcs: List[io.AbstractFile]): CompilerRunResult = {
      defaultRun {
        hook = Hook.indentation((level: Int) => {
          case ev if filt( ev ) => reportWithLevel(ev, level); NoResponse
        })
        hook hooking CompileWrapper.cc(srcs)
      }
    }
    
    def runTargeted(pos: Position, expandPos: Position): CompilerRunResult = {
      defaultRun {
        hook = Hook.indentation((level: Int) => {
          case ev if filt( ev ) => reportWithLevel(ev, level)(expandPos); NoResponse
        })
        hook hooking CompileWrapper.targetC(pos)
      }
    }
    
    private def defaultRun[T](compile: => T): CompilerRunResult = {
      EV.resetEventsCounter()
      _root = createNode(DummyRootEvent, null)(NoPosition)
      _errorNodes = Nil
      previousLevel = -1
      currentNodes.push((_root, previousLevel))
      compile
      InstrumentedRunResult(_root, _errorNodes.reverse)
    }
  }

  
  private case class InstrumentedRunResult(
      root: BaseTreeNode[EventNode],
      goals: List[BaseTreeNode[EventNode]])
    extends CompilerRunResult
  
  object CompileWrapper {
    def cc(files: List[io.AbstractFile]): Boolean = {
      println("[compiling] " + files.map(_.name))
      global.run(files)
      !global.reporter.hasErrors
    }
    
    def targetC(pos: global.Position): Boolean = {
      println("[targeted compile] " + pos)
      global.targetDebugAt(pos)
      !global.reporter.hasErrors
    }
  }
}