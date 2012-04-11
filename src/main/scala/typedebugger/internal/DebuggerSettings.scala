package scala.typedebugger
package internal

trait DebuggerSettings {
  self: scala.tools.nsc.settings.MutableSettings =>
  
  val fullTypechecking = BooleanSetting("-XfullTypecheck", "Type debugger option to show the whole tree")
  val debugTD          = StringSetting ("-YdebugTD", "level", "Debug level for type debugger", "")
  val reportSoftErrors = BooleanSetting("-XwithSoftErrors", "Focus type debugger on soft errors as well as the hard ones")
  val advancedDebug    = BooleanSetting("-Xadvanced", "Show debugging for synthetics and more advanced features")
  val withTargetThrow  = BooleanSetting("-Ythrowtarget", "Abrupt debugging immediately after typechecking the selected tree")
  val logEvents        = PathSetting   ("-YlogActivity", "Log all the user actions that were done while running type debugger", "/dev/null")
}

