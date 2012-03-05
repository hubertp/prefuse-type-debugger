package scala.typedebugger
package internal

trait CompilerInfo {
  val global: scala.tools.nsc.Global with Snapshots
  val settings: scala.tools.nsc.Settings with TypeDebuggerSettings
}