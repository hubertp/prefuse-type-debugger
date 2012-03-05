package scala.typedebugger
package internal

import scala.tools.nsc.event.EventsGlobal
import scala.tools.nsc.Global
import scala.tools.nsc.event.HookLoader

trait DebuggerGlobal extends EventsGlobal {
  outer: Global =>
  
  trait DebuggerStrings extends Strings {
    self: EventModel =>
      
    // TODO provide custom printing
    abstract override protected def anyStringInternal(x: Any): String = x match {
      case x: Tree => super.treeString(x)
      case _ => super.anyStringInternal(x)
    }
  }
  
  override def EVGlobal: EventModel with EventPostInit = new EVGlobal with DebuggerStrings {
    override def instrumentingOn = true
  }
}