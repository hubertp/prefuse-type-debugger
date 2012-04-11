package scala.typedebugger
package internal

import scala.tools.nsc.io
import scala.collection.mutable.{ ListBuffer, Stack }

trait StructureBuilders {
  self: CompilerInfo with IStructure with CompilationTools =>
    
  // Wrapper around the compiler that logs all the events
  // and creates the necessary structure (independent of UI)
  class CompilerRunWithEvents(nodesLabel: String, filt: global.EV.Filter) extends CompilerWithEventInfo {
    import global.{EV, Position, NoPosition}
    import EV._
    
    //type ENode = EventNode
    
    private[this] var _root: BaseTreeNode[EventNode] = _
    
    private[this] var errorNodes: List[BaseTreeNode[EventNode]] = Nil

    // List of nodes that are currently open
    private val currentNodes = new Stack[(BaseTreeNode[EventNode], Int)]()
    var previousLevel: Int = -1 // Start at root
    
    // Hook that enables us to collect all the events
    private var hook: Hook.IndentationHook = _
    
    private def createNode(ev: Event, parentENode: BaseTreeNode[EventNode])
                           (implicit statPos: Position): BaseTreeNode[EventNode] = {
      val evNode = new EventNode(ev, new ListBuffer(), if (parentENode == null) None else Some(parentENode))
      // We want them in order of appearance
      ev match {
        case _: HardErrorEvent =>
          errorNodes = evNode::errorNodes
        case e: ContextTypeError if (e.errType == ErrorLevel.Hard) =>
          errorNodes = evNode::errorNodes
        case e: TyperTyped if statPos != NoPosition && e.tree.pos.sameRange(statPos) =>
          /*e.expl match {
            case expl: StatementExplanation if expl.stat.pos.sameRange(statPos) =>
              errorNodes = evNode::errorNodes
            case _ =>
          }*/
          errorNodes = evNode::errorNodes
        case _ =>
      }
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
          if (top.children.isEmpty) {
            // This is the first 
            top.children += createNode(ev, top)
          } else {
            val last = top.children.last
            ev match {
              case _: DoneBlock =>
                // Don't push if it is at the same time a done block
              case _            =>
                currentNodes.push(last)
            }
            //assert(last.evs == Nil, "Last is not Nil: " + last.evs.mkString(",") + " want to add " + ev + " " + ev.getClass)
            last.children += createNode(ev, last)
            //top.children += createNode(ev, top)
          }
          previousLevel = level
  
        case rec: ExceptionRecoveryEvent =>
          assert(false) // TODO for now
          assert(previousLevel == level, "recovering from an exception")
          // Exception occured, need to backtrack to some previous opening
          while (currentNodes.top._1.ev != rec.lastOpen)
            currentNodes.pop()
            
          val baseLevel = currentNodes.top._2
          previousLevel = baseLevel
          hook.resetIndentation(baseLevel)
          
        case _: DoneBlock  =>
          assert(currentNodes.nonEmpty,
                  "stack of current nodes cannot be empty on end of the block for " + ev + " " + ev.getClass)
          val top = currentNodes.pop()
          top.children += createNode(ev, top)
        
        case _             =>
          previousLevel = level
/*          ev match {
            // TODO: Not necessary anymore?
            case _: SecondTryTypedApplyStartTyper =>
              assert(false)
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
          }*/
          // this also handles the case when we just parsed DoneBlock
          assert(!currentNodes.isEmpty)
          val top = currentNodes.top
          top.children += createNode(ev, top)
      }
    }
  
    //def pf(fxn: Event =>? Boolean): Unit = apply(Filter pf fxn)
    def run(srcs: List[io.AbstractFile]): CompilerRunResult = {
      EV.resetEventsCounter()
      // reset the intermediate structure
      _root = createNode(DummyRootEvent, null)(NoPosition)
      errorNodes = Nil
      previousLevel = -1
      currentNodes.push((_root, previousLevel))
      hook = Hook.indentation((level: Int) => {
        case ev if filt( ev )=> reportWithLevel(ev, level); NoResponse
      })
      hook hooking CompileWrapper.cc(srcs)
      InstrumentedRunResult(_root, errorNodes.reverse)
    }
    
    def runTargeted(pos: Position, expandPos: Position): CompilerRunResult = {
      EV.resetEventsCounter()
      _root = createNode(DummyRootEvent, null)(NoPosition)
      errorNodes = Nil
      previousLevel = -1
      currentNodes.push((_root, previousLevel))
      hook = Hook.indentation((level: Int) => {
        case ev if filt( ev )=> reportWithLevel(ev, level)(expandPos); NoResponse
      })
      
      hook hooking CompileWrapper.targetC(pos)
      InstrumentedRunResult(_root, errorNodes.reverse)
    }
  }
  
  private case object DummyRootEvent extends global.EV.Event {
    def tag = "dummy-root"
    override val phase = scala.tools.nsc.NoPhase
    override val unit = global.NoCompilationUnit
    override val file = None
    override def defaultPos = global.NoPosition
    override def pos = global.NoPosition
    def participants = List()
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
      !global.reporter.hasErrors // is that needed?
    }
  }
}