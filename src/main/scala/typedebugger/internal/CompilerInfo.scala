package scala.typedebugger
package internal

trait CompilerInfo {
  val global: scala.tools.nsc.Global
  val settings: scala.tools.nsc.Settings with TypeDebuggerSettings
}


trait TypeDebuggerSettings {
  self: scala.tools.nsc.settings.MutableSettings =>
  
  // TODO improve 
  val fullTypechecking = BooleanSetting("-XfullTypecheck", "Type debugger option to show the whole tree")
  val debugTD          = BooleanSetting("-YdebugTD", "Debug option for type debugger")
  val reportSoftErrors = BooleanSetting("-XwithSoftErrors", "Focus type debugger on soft errors as well as the hard ones")
}