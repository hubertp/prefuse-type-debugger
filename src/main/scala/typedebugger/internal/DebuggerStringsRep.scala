package scala.typedebugger
package internal

trait DebuggerStringsRep {
  dGlobal: DebuggerGlobal =>
    
  trait DebuggerStrings {
    self: EventModel =>
    
    abstract override protected def anyStringInternal(x: Any): String = x match {
      case x: Tree => debuggerTreeString(x)
      case t: Type => debuggerTypeString(t) 
      case _       => self.anyStringInternal(x)
    }
    
    // todo: provide better printer for trees
    def debuggerTreeString(tree: Tree): String = tree match {
      case TypeApply(fun, args) =>  self.treeString(tree)
      case _ => self.treeString(tree)
    }
    
    def debuggerTypeString(tpe: Type, withKinds: Boolean = false): String = tpe match {
      case WildcardType           => "?"
      case ErrorType              => "Error"
      // explain non-value types
      case tp@MethodType(_, _)    => tp.safeToString
      case tp@PolyType(tparams, _) => tp.safeToString
      case tp@SingleType(_, _)    => tp.safeToString
      case tp:TypeRef             => tp.safeToString
      case tp@OverloadedType(pre, alts) => "Overloaded Type\n" + (alts map pre.memberType).mkString("", "\n<and>\n ", "")
      case tp@RefinedType(_, _)   => tp.safeToString
      case ConstantType(v)        => debuggerTypeString(v.tpe)
      case NoType                 => "NoType"
      case tp@TypeVar(_, _)       => "?" + tp.originName//tp.safeToString
      case _                      => typeString(tpe)
    }
  }

}