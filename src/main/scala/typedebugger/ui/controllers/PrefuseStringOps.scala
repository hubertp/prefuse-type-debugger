package scala.typedebugger
package ui
package controllers

import prefuse.visual.VisualItem

trait PrefuseStringOps {
  self: internal.CompilerInfo with UIUtils with stringops.StringOps with internal.PrefuseStructure =>
  
  import global._
  import EV._
    
  def fullStringOps(item: VisualItem): util.StringFormatter = {
    val node = asDataNode(item)
    fullStringOps(node.ev, node)
  }

  def fullStringOps(ev: Event, uiNode: UINode[PrefuseEventNode]): util.StringFormatter =
    if (ev != null) EventDescriptors(ev, uiNode).fullInfo else "Typecheck full tree" // root
  
  def shortStringOps(item: VisualItem): String = {
    val node = asDataNode(item) // use UI node only for the label descriptions for the moment
    shortStringOps(node.ev, node)
  }
  
  def shortStringOps(ev: Event, uiNode: UINode[PrefuseEventNode]): String = if (ev != null)
    ev match {
      case tpchecker:TyperTyped =>
        Explanations(tpchecker)
      case _ =>
        EventDescriptors(ev, uiNode).basicInfo
    }
    else "Typecheck full tree" // root
}