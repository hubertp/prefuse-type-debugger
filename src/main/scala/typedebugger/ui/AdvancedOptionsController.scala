package scala.typedebugger
package ui

import prefuse.data.Tuple

trait AdvancedOptionsController {
  def enableOption(v: Filtering.Value): Unit
  def disableOption(v: Filtering.Value): Unit
  def isOptionEnabled(t: Tuple): Boolean
  def isAdvancedOption(t: Tuple): Boolean  
}