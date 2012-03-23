package scala.typedebugger

import scala.tools.nsc.{ Global, CompilerCommand, Settings, io, interactive }
import scala.tools.nsc.reporters.{ ConsoleReporter }

object TypeDebuggerUI {
  def main(args: Array[String]) {
    // parse input: sources, cp, d
    val settings0 = new Settings() with TypeDebuggerSettings
    settings0.Yrangepos.value = true // redundant?
    settings0.stopAfter.value = List("typer")
    
    val command = new CompilerCommand(args.toList, settings0)
    val tb = new TypeBrowser {
      val global = new internal.DebuggerGlobal(settings0, new ConsoleReporter(settings0))
    }
    
    tb.compileAndShow(command.files, settings0)
  }
}