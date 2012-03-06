package scala.typedebugger
package ui

import prefuse.data.{Node, Tuple}
import prefuse.data.expression.{AbstractPredicate}
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
