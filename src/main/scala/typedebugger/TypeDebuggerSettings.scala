package scala.typedebugger

trait TypeDebuggerSettings {
  self: scala.tools.nsc.settings.MutableSettings =>
  
  val fullTypechecking = BooleanSetting("-XfullTypecheck", "Type debugger option to show the whole tree")
  val debugTD          = BooleanSetting("-YdebugTD", "Debug option for type debugger")
  val reportSoftErrors = BooleanSetting("-XwithSoftErrors", "Focus type debugger on soft errors as well as the hard ones")
  val advancedDebug    = BooleanSetting("-Xadvanced", "Show debugging for synthetics and more advanced features")
  val withTargetThrow  = BooleanSetting("-Ythrowtarget", "Abrupt debugging immediately after typechecking the selected tree")
}