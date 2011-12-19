package scala.typedebugger
package internal

trait CompilerInfo {
  val global: scala.tools.nsc.Global
  val DEBUG: Boolean
}