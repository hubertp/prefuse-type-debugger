package scala.typedebugger
package ui

import prefuse.data.{Node, Tuple}
import prefuse.data.expression.{AbstractPredicate, Predicate}
import prefuse.data.tuple.TupleSet
import prefuse.visual.{VisualItem, NodeItem}
import prefuse.action.Action
import prefuse.Visualization

import scala.collection.JavaConversions._

trait UIUtils {
  self: internal.CompilerInfo with internal.PrefuseStructure =>
    
  //type UINodeP = UINode[PrefuseEventNode]
  import UIConfig.{nodesLabel => label}
    
  // Search for visual item corresponding to node (i.e. Node ==> NodeItem)
  class VisualItemSearchPred(search: Node) extends AbstractPredicate {
    override def getBoolean(t: Tuple): Boolean = t match {
      case item: NodeItem if containsDataNode(t) =>
        asDataNode(t).pfuseNode == search
      case _ => false
    }
  }

  // todo: define in terms of unapply/apply
  def containsDataNode(t: Tuple): Boolean = t.canGet(label, COLUMN_PREFUSENODE_CLASS)
  def asDataNode(t: Tuple): UINode[PrefuseEventNode] = t.get(label).asInstanceOf[UINode[PrefuseEventNode]]
}

object UIConfig {
  val nodesLabel = "event.node"
}

class VisualizeNodes(groupName: String) extends Action {
  def run(frac: Double) {
    val target = m_vis.getFocusGroup(Visualization.FOCUS_ITEMS)
    val ts = m_vis.getFocusGroup(groupName)
    ts.tuples().foreach(n => target.addTuple(n.asInstanceOf[Tuple]))
  }
}

object PrefusePimping {

  implicit def tupleSetToIterator[T](ts: TupleSet): Iterator[T] = new Iterator[T] {
    val iter = ts.tuples()
    def hasNext: Boolean = iter.hasNext
    def next(): T = iter.next().asInstanceOf[T]
  }

  implicit def nodeItemWraper(nodeItem: NodeItem) = new NodeItemWrapper(nodeItem)
  class NodeItemWrapper(underlying: NodeItem) {
    def childEdges_[T]():java.util.Iterator[T] = underlying.childEdges().asInstanceOf[java.util.Iterator[T]]
    def outNeighbors_[T]():java.util.Iterator[T] = underlying.outNeighbors().asInstanceOf[java.util.Iterator[T]]
    def inEdges_[T]():java.util.Iterator[T] = underlying.inEdges().asInstanceOf[java.util.Iterator[T]]
    def children_[T]: java.util.Iterator[T] = underlying.children().asInstanceOf[java.util.Iterator[T]]
  }
  
  implicit def visualizationWrapper(vis: Visualization) = new VisualizationWrapper(vis)
  class VisualizationWrapper(underlying: Visualization) {
    def items_[T](pred: Predicate): java.util.Iterator[T] = underlying.items(pred).asInstanceOf[java.util.Iterator[T]]
    def items_[T](group: String): java.util.Iterator[T] = underlying.items(group).asInstanceOf[java.util.Iterator[T]]
    def items_[T](group: String, pred: Predicate): java.util.Iterator[T] = underlying.items(group, pred).asInstanceOf[java.util.Iterator[T]]
  }
  // that doesn't work, because type error comes first and there is no backtracking (why?)
  //implicit def childEdges[T](nodeItem: NodeItem): java.util.Iterator[T] = nodeItem.childEdges().asInstanceOf[java.util.Iterator[T]]
}
