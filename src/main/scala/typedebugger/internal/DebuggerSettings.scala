package scala.typedebugger
package internal

trait DebuggerSettings {
  self: scala.tools.nsc.settings.MutableSettings =>
  
  val fullTypechecking = BooleanSetting("-XfullTypecheck", "Type debugger option to show the whole tree")
  val reportSoftErrors = BooleanSetting("-XwithSoftErrors", "Focus type debugger on soft errors as well as the hard ones")
  val advancedDebug    = BooleanSetting("-Xadvanced", "Show debugging for synthetics and more advanced features")
  
  val debugTD          = StringSetting ("-YdebugTD", "level", "Debug level for type debugger", "")
  val withTargetThrow  = BooleanSetting("-Ythrowtarget", "Abrupt debugging immediately after typechecking the selected tree")
  val logEvents        = PathSetting   ("-YlogActivity", "Log all the user actions that were done while running type debugger", "/dev/null")
  val detached         = BooleanSetting("-Ydetached", "Type debugger does not start UI, only for testing")
  val noFiltering      = BooleanSetting("-YnoFilter", "Use this flag if you want type debugger to *not* filter out inessential nodes")
}

