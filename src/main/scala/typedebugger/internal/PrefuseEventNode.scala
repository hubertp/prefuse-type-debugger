package scala.typedebugger
package internal

import prefuse.data.Node
import scala.collection.mutable
import mutable.{ ListBuffer }

trait PrefuseStructure extends IStructure {
  self: CompilerInfo with processing.StringOps with EventFiltering =>
    
  import global._
  import EV._
  
  val COLUMN_PREFUSENODE_CLASS = (new PrefuseEventNode(null, null, null)).getClass
  
  trait UINodeLike[T, Container[X]] extends BaseTreeNodeLike[T, Container] {
    val pfuseNode: Node
    def fullInfo: util.StringFormatter
    def advanced: Boolean
  }
  
  trait UINode[T] extends UINodeLike[T, UINode]

  class PrefuseEventNode(val ev: Event,
                         val parent: Option[UINode[PrefuseEventNode]],
                         val pfuseNode: Node) extends UINode[PrefuseEventNode] {
    val children = ListBuffer[UINode[PrefuseEventNode]]()
    
    override def toString =
      if (ev != null) {
        ev match {
          case tpchecker:TyperTyped =>
            Explanations(tpchecker)
          case _ =>
            EventDescriptors(ev).basicInfo
        }
      } else "Typecheck full tree" // root

    def fullInfo =
      if (ev != null)
        ev match {
          case evTyped: TyperTyped =>
            //TODO This still needs adjustment
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
    
    // Determine initial state for the node
    // This can change by enabling options explicitly
    lazy val advanced: Boolean = FilteringOps.map.isDefinedAt(ev)
  }
}