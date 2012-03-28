package scala.typedebugger
package internal

trait DebuggerStringsRep {
  dGlobal: DebuggerGlobal =>
    
  trait DebuggerStrings {
    self: EventModel =>
    
    abstract override protected def anyStringInternal(x: Any): String = x match {
      case x: Tree => self.treeString(x)
      case _       => self.anyStringInternal(x)
    }
  }

}