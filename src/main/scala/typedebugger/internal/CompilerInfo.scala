package scala.typedebugger
package internal

trait CompilerInfo {
  val global: scala.tools.nsc.Global
  val settings: scala.tools.nsc.Settings with TypeDebuggerSettings
  val DEBUG: Boolean
}


trait TypeDebuggerSettings {
  self: scala.tools.nsc.settings.MutableSettings =>
  
  // TODO improve 
  val fullTypechecking = BooleanSetting("-XfullTypecheck", "Type debugger option to show the whole tree")
}