package scala.typedebugger
package internal

import prefuse.data.Node
import scala.collection.mutable
import mutable.{ ListBuffer }

trait PrefuseStructure {
  self: CompilerInfo with IStructure with processing.StringOps =>
    
  import global._
  import EV._
  
  trait UINodeLike[T, Container[X]] extends BaseTreeNodeLike[T, Container] {
    val pfuseNode: Node
    def fullInfo: String
    var goal: Boolean
  }
  
  trait UINode[T] extends UINodeLike[T, UINode]

  class PrefuseEventNode(val ev: global.EV.Event,
                         val parent: Option[UINode[PrefuseEventNode]],
                         val pfuseNode: Node) extends UINode[PrefuseEventNode] {
    val children = ListBuffer[UINode[PrefuseEventNode]]()
    var goal = false // for caching purposes so that we don't have to constantly
                     // check neighbors
    
    // TODO refactor to a separate place that handles all the UI stuff
    override def toString =
      if (ev != null) {
        ev match {
          case tpchecker:TyperTyped =>
            Explanations(tpchecker)
          case _ =>
            Events(ev)._1
        }
      } else "Typecheck full tree" // root
        
    // TODO this should go
    def fullInfo =
      if (ev != null)
        ev match {
          case evTyped: TyperTyped =>
            //TODO This still needs adjustment
            val tpe = if (evTyped.tree.tpe != null) evTyped.tree.tpe else if (evTyped.tree.symbol != null) evTyped.tree.symbol.tpe else null
            /*(evTyped formattedString PrefuseEventNode.fmtFull) + "\n" + 
            "Type of tree: [ " + tpe + " ] expected: [ " + evTyped.pt + " ]" + 
            "\nDebugging info: " + evTyped.tree.getClass +
            "\nEvent toString: " + evTyped.eventString*/
            evTyped.expl + "\n\n" +
            "Typechecking tree: \n " +
            evTyped.tree + "\n\n" +
            "\nExpected type: " + (if (evTyped.pt == WildcardType) "None" else anyString(evTyped.pt)) +
            (if (tpe != null) "\nType of tree set to: " + anyString(tpe) else " Tree not yet typed")
            //"\nTree class " + evTyped.tree.getClass + " pos " + evTyped.tree.pos

          case _ =>
            Events(ev)._2
            //ev formattedString Formatting.fmtFull
        }
      else "Typecheck full tree" // root
  }
}