package scala.typedebugger
package internal

import scala.tools.nsc.Settings

trait CompilerInfo {
  val global: DebuggerGlobal
  def settings: Settings with TypeDebuggerSettings = global.settings
  def debug(msg: => String, kind: String): Unit = global.debug(msg, kind)
  def debug(msg: => String): Unit = global.debug(msg)
}