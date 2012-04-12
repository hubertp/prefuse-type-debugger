package scala.typedebugger
package internal

import scala.collection.mutable.ListBuffer

trait IStructure {
  outer: CompilerInfo =>

  import global.EV.Event
  
  // TODO: +T
  trait BaseTreeNodeLike[T, Container[X]] {
    def ev: Event
    val children: ListBuffer[Container[T]]
    def parent: Option[Container[T]]
  }
  
  trait BaseTreeNode[T] extends BaseTreeNodeLike[T, BaseTreeNode]

  class EventNode(val ev: Event,
                  val children: ListBuffer[BaseTreeNode[EventNode]],
                  val parent: Option[BaseTreeNode[EventNode]]
  ) extends BaseTreeNode[EventNode]

}