package scala.typedebugger
package util


trait DebuggerUtils {
  self: internal.CompilerInfo =>
    
  import global._
  
  class TreeOps(t: Tree) {
    def isErroneousApply = t match {
      case Apply(fun, args) =>
        fun.isErroneous || args.exists(_.isErroneous)
      case _                =>
        false
    }
  }
  
  implicit def toWrapperTree(t: Tree): TreeOps = new TreeOps(t)
}

object Formatting {
  //val fmtFull = "[%ph] [%tg] %ev %po %id" // TODO this should be configurable
  //val fmtFull = "[%ph] [%tg] %ev" // TODO this should be configurable
  val fmtFull = "%ev"
  val fmt = "%ln"
  val maxTypeLength = 50 // arbitrary const, used for UI reasons
}