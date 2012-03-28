package scala.typedebugger
package internal

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive
import scala.tools.nsc.io
import scala.tools.nsc.util.SourceFile

import scala.collection.mutable


trait CompilerInfo {
  val global: DebuggerGlobal
  def settings: Settings with TypeDebuggerSettings = global.settings
  def debug(msg: String) = if (global.settings.debugTD.value) println("[debug] " + msg)
  def targetedCompile(pos: global.Position): Unit
}

// todo move to separate file
trait Tools {
  self: CompilerInfo =>
    
  trait CompilerWithEventInfo {
    def run(srcs: List[io.AbstractFile]): Boolean
    def runTargeted(pos: global.Position, expandPos: global.Position): Boolean
  }
}
