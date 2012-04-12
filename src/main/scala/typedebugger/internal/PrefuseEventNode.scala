package scala.typedebugger
package internal

import prefuse.data.Node
import scala.collection.mutable.ListBuffer

trait PrefuseStructure extends IStructure {
  self: CompilerInfo with EventFiltering =>
    
  import global.EV.Event
  
  trait UINodeLike[T, Container[X]] extends BaseTreeNodeLike[T, Container] {
    val pfuseNode: Node
    def advanced: Boolean
  }
  
  trait UINode[T] extends UINodeLike[T, UINode]

  class PrefuseEventNode(val ev: Event,
                         val parent: Option[UINode[PrefuseEventNode]],
                         val pfuseNode: Node) extends UINode[PrefuseEventNode] {
    val children = ListBuffer[UINode[PrefuseEventNode]]()
    
    override def toString: String = "[prefuse node] " + ev
    
    lazy val advanced: Boolean = FilteringOps.map.isDefinedAt(ev)
  }
}