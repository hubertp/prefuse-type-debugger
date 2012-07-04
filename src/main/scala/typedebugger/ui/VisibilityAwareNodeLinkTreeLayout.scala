package scala.typedebugger
package ui

import prefuse.data.{Graph, Table, Node, Tuple, Edge, Tree}
import prefuse.action.layout.graph.NodeLinkTreeLayout
import prefuse.visual.{VisualItem, NodeItem, EdgeItem}

import scala.collection.JavaConversions._

abstract class VisibilityAwareNodeLinkTreeLayout(
  orientation: Int, dspace: Double, bspace: Double, tspace: Double,
  dataGroupName: String, visibleGroup: String)
  extends NodeLinkTreeLayout(dataGroupName) {
  
  import PrefuseDisplay.toVisualNode
  import PrefusePimping.nodeItemWraper
  
  def realRoot(): Node
  def isCustomNodeVisible(n: NodeItem): Boolean
  
  // Anchor the layout root at the first error
  // or show the synthetic root
  // whenever we expand the type tree we update the root
  override def getLayoutRoot() = {      
    var item:Node = realRoot()//lst.getRoot()
    if (item == null) {
      super.getLayoutRoot()
    } else {
      while (item.getParent() != null && toVisualNode(item.getParent, m_vis, dataGroupName).isVisible)
        item = item.getParent()
      toVisualNode(item, m_vis, dataGroupName)
    }
  }
  
  override def getGraph(): Graph = {
    m_vis.getGroup(visibleGroup).asInstanceOf[Graph]
  }
  
  // Methods below are a direct copy of the original behaviour with the exception
  // that we take into account visible/advanced nodes when calculating
  // the proper layout
  // @see prefuse.action.layout.graph.NodeLinkTreeLayout for details
  override def firstWalk(n: NodeItem, num: Int, depth: Int) {
    val np = getParams(n)
    np.number = num
    updateDepths(depth, n)
      
    val expanded = n.isExpanded()
    if ( Proxy.getChildCount(n) == 0 || !expanded ) {
      val l = Proxy.getPreviousSibling(n)
      np.prelim = if ( l == null ) 0 else (getParams(l).prelim + spacing(l,n,true))
    } else if ( expanded ) {
      val leftMost = Proxy.getFirstChild(n)
      val rightMost = Proxy.getLastChild(n)
      var defaultAncestor = leftMost
      var c = leftMost
      var i = 0
      while (c != null) {
        firstWalk(c, i, depth+1)
        defaultAncestor = apportion(c, defaultAncestor)
        i += 1
        c = Proxy.getNextSibling(c)
      }
          
      executeShifts(n)
          
      val midpoint = 0.5 *
         (getParams(leftMost).prelim + getParams(rightMost).prelim)
          
      val left = Proxy.getPreviousSibling(n)
      if ( left != null ) {
        np.prelim = getParams(left).prelim + spacing(left, n, true)
        np.mod = np.prelim - midpoint
      } else {
        np.prelim = midpoint
      }
    }
  }
  
  override def apportion(v: NodeItem, a: NodeItem): NodeItem = {        
    val w = Proxy.getPreviousSibling(v)
    if ( w != null ) {
      var vip, vim, vop, vom: NodeItem = null
      var sip, sim, sop, som: Double = 0

      vip = v; vop = v

      vim = w
      vom = Proxy.getFirstChild(vip.getParent().asInstanceOf[NodeItem])

      sip = getParams(vip).mod
      sop = getParams(vop).mod
      sim = getParams(vim).mod
      som = getParams(vom).mod
      
      var nr = nextRight(vim) // TODO: need to use our version
      var nl = nextLeft(vip)  // same here
      while ( nr != null && nl != null ) {
        vim = nr
        vip = nl
        vom = nextLeft(vom)
        vop = nextRight(vop)
        getParams(vop).ancestor = v
        val shift: Double = (getParams(vim).prelim + sim) - 
            (getParams(vip).prelim + sip) + spacing(vim,vip,false)
        if ( shift > 0 ) {
            moveSubtree(ancestor(vim,v,a), v, shift)
            sip += shift
            sop += shift
        }
        sim += getParams(vim).mod
        sip += getParams(vip).mod
        som += getParams(vom).mod
        sop += getParams(vop).mod
        
        nr = nextRight(vim)
        nl = nextLeft(vip)
      }
      if ( nr != null && nextRight(vop) == null ) {
        val vopp = getParams(vop)
        vopp.thread = nr
        vopp.mod += sim - sop
      }
      if ( nl != null && nextLeft(vom) == null ) {
        val vomp = getParams(vom)
        vomp.thread = nl
        vomp.mod += sip - som
        v
      } else a
    } else a
  }
  
  override def nextLeft(n: NodeItem): NodeItem = {
    var c: NodeItem = null
    if ( n.isExpanded() && Proxy.isValid(n) ) c = Proxy.getFirstChild(n)
    if (c != null) c else getParams(n).thread
  }
  
  override def nextRight(n: NodeItem): NodeItem = {
    var c: NodeItem = null
    if ( n.isExpanded() && Proxy.isValid(n) ) c = Proxy.getLastChild(n)
    if (c != null) c else getParams(n).thread
  }
  
  override def executeShifts(n: NodeItem) {
    var shift, change: Double = 0
    var c = Proxy.getLastChild(n)
    while (c != null) {
      val cp = getParams(c)
      cp.prelim += shift
      cp.mod += shift
      change += cp.change
      shift += cp.shift + change
      c = Proxy.getPreviousSibling(c)
    }
  }

  override def ancestor(vim: NodeItem, v: NodeItem, a: NodeItem): NodeItem = {
    val p:NodeItem = v.getParent().asInstanceOf[NodeItem]
    val vimp = getParams(vim)
    if (vimp.ancestor.getParent() == p) vimp.ancestor else a
  }
  
  override def secondWalk(n: NodeItem, p: NodeItem, m: Double, depth0: Int) {
    val np = getParams(n)
    var depth = depth0
    setBreadth(n, p, np.prelim + m)
    setDepth(n, p, m_depths(depth))
    
    if ( n.isExpanded() && Proxy.isValid(n) ) {
      depth += 1
      var c = Proxy.getFirstChild(n)
      while (c != null) {
        secondWalk(c, n, m + np.mod, depth)
        c = Proxy.getNextSibling(c)
      }
    }
    np.clear()
  }
  
  //end of custom layout, specialized NodeLinkTreeLayout for our needs
  
  // Provide main custom operation for navigating between nodes, 
  // that takes into account visibility requirements of the type debugger
  object Proxy {
    def isValid(node: NodeItem): Boolean =
      node.isVisible && isCustomNodeVisible(node)
    
    def getChildCount(underlying: NodeItem): Int =
      underlying.children_[NodeItem].toList.filter(isValid).size
    
    def getFirstChild(underlying: NodeItem): NodeItem = {
      val first = underlying.getFirstChild().asInstanceOf[NodeItem]
      if (first != null) {
        if (!isValid(first)) Proxy.getNextSibling(first) else first
      } else null
    }
    
    def getLastChild(underlying: NodeItem): NodeItem = {
      val last = underlying.getLastChild().asInstanceOf[NodeItem]
      if (last != null) {
        if (!isValid(last)) Proxy.getPreviousSibling(last) else last
      } else null
    }
    
    def getPreviousSibling(underlying: NodeItem): NodeItem = {
      var previous = underlying.getPreviousSibling().asInstanceOf[NodeItem]
      while (previous != null && !isValid(previous))
        previous = previous.getPreviousSibling().asInstanceOf[NodeItem]
      previous
    }
    
    def getNextSibling(underlying: NodeItem): NodeItem = {
      var next = underlying.getNextSibling().asInstanceOf[NodeItem]
      while (next != null && !isValid(next))
        next = next.getNextSibling().asInstanceOf[NodeItem]
      next
    }
  }
}