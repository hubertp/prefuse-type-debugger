package scala.typedebugger
package internal

import scala.tools.nsc.{Global, Settings}

trait CompilerInfo {
  val global: Global with Snapshots
  val settings: Settings with TypeDebuggerSettings

  def debug(msg: String) = if (settings.debugTD.value) println("[debug] " + msg)
}