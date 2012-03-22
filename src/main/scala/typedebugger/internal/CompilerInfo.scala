package scala.typedebugger
package internal

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive
import scala.tools.nsc.io
import scala.tools.nsc.util.SourceFile

import scala.collection.mutable


trait CompilerInfo {
  val global: Global with Snapshots with interactive.RangePositions with DebuggerCompilationUnits
    with DebuggerGlobal with DebuggerPositions
  
  val settings: Settings with TypeDebuggerSettings // todo remove, superseded by global.settings
  def debug(msg: String) = if (settings.debugTD.value) println("[debug] " + msg)
  def targetedCompile(pos: global.Position): Unit
}

// todo move to separate file
trait Tools {
  self: CompilerInfo =>
    
  trait CompilerWithInstrumentation {
    def run(srcs: List[io.AbstractFile]): Boolean
    def runTargeted(pos: global.Position, statPos: global.Position): Boolean
  }
}
