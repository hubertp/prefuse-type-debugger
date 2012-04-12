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

  def fullStringOps(ev: Event): util.StringFormatter = if (ev != null)
    ev match {
      case evTyped: TyperTyped =>
        val tpe = if (evTyped.tree.tpe != null) evTyped.tree.tpe else if (evTyped.tree.symbol != null) evTyped.tree.symbol.tpe else null
        evTyped.expl + "\n\n" +
        "Typechecking tree: \n " +
        snapshotAnyString(evTyped.tree)(evTyped.time) + "\n\n" +
        (if (settings.debugTD.value == "event" && evTyped.tree != null) "Tree class " + evTyped.tree.getClass + " with sym " + evTyped.tree.symbol else "") +
        "\nExpected type: " + (if (evTyped.pt == WildcardType) "None" else snapshotAnyString(evTyped.pt)(evTyped.time)) +
        (if (tpe != null) "\nType of tree set to: " + snapshotAnyString(tpe)(evTyped.time) else " Tree not yet typed")

      case _ =>
        EventDescriptors(ev).fullInfo
    }
    else "Typecheck full tree" // root
  
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