package scala.typedebugger
package internal

import scala.tools.nsc.Global
import scala.tools.nsc.util.SourceFile

// Simplified RichCompilationUnit
trait DebuggerCompilationUnits {
  self: Global =>
    
    
  final val NotLoaded = -2
  final val JustParsed = -1
  final val TypeChecked = 0
  
  class DebuggerCompilationUnit(source: SourceFile) extends CompilationUnit(source) {
    var status: Int = NotLoaded
    
    var _targetPos: Position = NoPosition
    override def targetPos: Position = _targetPos
    def targetPos_=(p: Position) { _targetPos = p }
  }
}