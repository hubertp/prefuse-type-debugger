package scala.typedebugger
package ui
package controllers

import prefuse.visual.VisualItem

trait PrefuseStringOps {
  self: internal.CompilerInfo with UIUtils with stringops.StringOps with internal.PrefuseStructure =>
  
  import global._
  import EV._
    
  def fullStringOps(item: VisualItem): util.StringFormatter =
    fullStringOps(asDataNode(item).ev)

  def fullStringOps(ev: Event): util.StringFormatter =
    if (ev != null) EventDescriptors(ev).fullInfo else "Typecheck full tree" // root
  
  def shortStringOps(item: VisualItem): String =
    shortStringOps(asDataNode(item).ev)
  
  def shortStringOps(ev: Event): String = if (ev != null)
    ev match {
      case tpchecker:TyperTyped =>
        Explanations(tpchecker)
      case _ =>
        EventDescriptors(ev).basicInfo
    }
    else "Typecheck full tree" // root
}