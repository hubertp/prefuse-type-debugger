package scala.typedebugger
package internal

trait DebuggerStringsRep {
  dGlobal: DebuggerGlobal =>
    
  trait DebuggerStrings {
    self: EventModel =>
    
    abstract override protected def anyStringInternal(x: Any): String = x match {
      case x: Tree => self.treeString(x)
      case t: Type => debuggerTypeString(t) 
      case _       => self.anyStringInternal(x)
    }
    
    def debuggerTypeString(tpe: Type, withKinds: Boolean = false): String = tpe match {
      case WildcardType           => "?"
      case ErrorType              => "Error"
      case tp@MethodType(_, _)    => tp.safeToString // express as A => B ?
      case tp:TypeRef             => tp.safeToString 
      case ConstantType(v)        => debuggerTypeString(v.tpe)
      case NoType                 => "NoType"
      case _                      => typeString(tpe)
    }
  }

}