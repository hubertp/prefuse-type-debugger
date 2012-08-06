package scala.typedebugger
package internal

import prefuse.data.Node
import scala.collection.mutable.ListBuffer

trait PrefuseStructure extends IStructure {
  self: CompilerInfo with EventFiltering =>
    
  import global.EV.Event
  
  trait UINodeLike[T, Container[X]] extends BaseTreeNodeLike[T, Container] {
    def advanced: Boolean
    def updateParent(newParent: Container[T])
    def pfuseNode: Node
    def updatePfuseNode(node: Node)
  }
  
  trait UINode[T] extends UINodeLike[T, UINode]

  class PrefuseEventNode(val ev: Event,
                         parent0: Option[UINode[PrefuseEventNode]]) extends UINode[PrefuseEventNode] {
    val children = ListBuffer[UINode[PrefuseEventNode]]()
    
    private var _parent = parent0
    def parent = _parent
    def updateParent(newParent: UINode[PrefuseEventNode]) {
      _parent = Some(newParent)
    }
    
    private var _pfuseNode: Node = null
    def pfuseNode = _pfuseNode
    def updatePfuseNode(node: Node) {
      _pfuseNode = node
    }
    
    override def toString: String = "[prefuse node] " + ev + " " + ev.getClass
    
    lazy val advanced: Boolean = FilteringOps.map.isDefinedAt(ev)
  }
}