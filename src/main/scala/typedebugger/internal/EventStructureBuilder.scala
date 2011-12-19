package scala.typedebugger
package internal

import scala.tools.nsc.io
import scala.collection.mutable.{ ListBuffer, Stack }

trait StructureBuilders {
  self: CompilerInfo with IStructure =>

  // Wrapper around the compiler that logs all the events
  // and creates the necessary structure (independent of UI)
  class EventTreeStructureBuilder(srcs: List[String], nodesLabel: String) {
    import global.EV
    import EV._
    
    //type ENode = EventNode
    
    private var _root: BaseTreeNode[EventNode] = _
    def root = _root
    
    private var errorNodes: List[BaseTreeNode[EventNode]] = Nil
    def initialGoals = errorNodes.reverse

    
    private val currentNodes = new Stack[(BaseTreeNode[EventNode], Int)]()
    var previousLevel: Int = -1 // Start at root
    
    // Hook that enables us to collect all the events
    private var hook: Hook.IndentationHook = _
    
    private def createNode(ev: Event, parentENode: BaseTreeNode[EventNode]): BaseTreeNode[EventNode] = {
      val evNode = new EventNode(ev, new ListBuffer(), if (parentENode == null) None else Some(parentENode))
      // We want them in order of appearance
      ev match {
        case _: HardErrorEvent =>
          errorNodes = evNode::errorNodes
        case e:ContextTypeError if (e.errType == ErrorLevel.Hard) =>
          errorNodes = evNode::errorNodes
        case _ =>
      }
      evNode
    }
    
    //private def updateChildren(node: BaseTreeNode[EventNode], child0: BaseTreeNode[EventNode]) {
    //  node.children += child0
      //node.children = child0 +: node.children
    //}
   
    // analyze the logged events and build necessary structure for the tree
    def reportWithLevel(ev: Event, level: Int) {
      // This relies on the fact that done blocks are not filtered out
  //      assert(previousLevel <= level, "prev: " + previousLevel + " level " + level)
      implicit def nodeWithLevel(a: BaseTreeNode[EventNode]): (BaseTreeNode[EventNode], Int) = (a, level)
      implicit def onlyNode(a: Tuple2[BaseTreeNode[EventNode], Int]) = a._1

      ev match {
        case _ if previousLevel < level => // instead use level indication
          val top = currentNodes.top
  
          if (top.children.isEmpty) {
            top.children += createNode(ev, top)
          } else {
            val last = top.children.last
            ev match {
              case _: DoneBlock =>
                // Don't push if it is at the same time a done block
              case _            =>
                currentNodes.push(nodeWithLevel(last))
            }
             
            //assert(last.evs == Nil, "Last is not Nil: " + last.evs.mkString(",") + " want to add " + ev + " " + ev.getClass)
            last.children += createNode(ev, last)
          }
          previousLevel = level
  
        case rec: ExceptionRecoveryEvent =>
          assert(previousLevel == level, "recovering from an exception")
          // Exception occured, need to backtrack to some previous opening
          while (currentNodes.top._1.ev != rec.lastOpen)
            currentNodes.pop()
            
          val baseLevel = currentNodes.top._2
          previousLevel = baseLevel
          hook.resetIndentation(baseLevel)
          
        case _: DoneBlock  =>
          assert(!currentNodes.isEmpty,
                  "stack of current nodes cannot be empty on end of the block for " + ev + " " + ev.getClass)
          val top = currentNodes.pop()
          top.children += createNode(ev, top)
        
        case _             =>
          ev match {
            // TODO: Not necessary anymore?
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
          val top = currentNodes.top
          top.children += createNode(ev, top)
      }
    }
  
    def pf(fxn: Event =>? Boolean): Unit = apply(Filter pf fxn)
    def apply(filt: Filter): Unit = {
      EV.resetEventsCounter()
      // reset the intermediate structure
      _root = createNode(null, null)
      previousLevel = -1
      currentNodes.push((_root, previousLevel))
      hook = Hook.indentation((level: Int) => {
        case ev if filt(ev)=> reportWithLevel(ev, level); NoResponse
      })
      hook hooking CompileWrapper.cc(srcs)
    }
  }
  
  object CompileWrapper {
    private def sources(srcRoots: List[String]): List[String] = {
      val srcs = new ListBuffer[String]
      srcRoots foreach { p =>
        io.Path(p).walk foreach { x =>
          if (x.isFile && x.hasExtension("scala", "java"))
            srcs += x.path
        }
      }
      srcs.toList.distinct
    }
  
    def cc(paths: List[String]): Boolean = {
      val run = new global.Run
      val srcs = sources(paths)
      println("[compiling] " + srcs)
      run compile srcs
      !global.reporter.hasErrors
    }
  }
}