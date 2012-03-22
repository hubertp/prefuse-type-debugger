package scala.typedebugger
package internal

import scala.tools.nsc.Global
import scala.tools.nsc.util.SourceFile

// Simplified RichCompilationUnit
trait DebuggerCompilationUnits {
  self: Global =>
  
  class DebuggerCompilationUnit(source: SourceFile) extends CompilationUnit(source) {    
    var _targetPos: Position = NoPosition
    override def targetPos: Position = _targetPos
    def targetPos_=(p: Position) { _targetPos = p }
  }
}