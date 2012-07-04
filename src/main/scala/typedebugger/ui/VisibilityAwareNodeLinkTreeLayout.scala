package scala.typedebugger
package ui

import prefuse.data.{Graph, Table, Node, Tuple, Edge, Tree}
import prefuse.action.layout.graph.{NodeLinkTreeLayout, Params}
import prefuse.visual.{VisualItem, NodeItem, EdgeItem}

import scala.collection.JavaConversions._
import scala.annotation.tailrec

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
  
  override def getGraph(): Graph =
    m_vis.getGroup(visibleGroup).asInstanceOf[Graph]
  
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
      np.prelim = Proxy.getPreviousSibling(n) match {
        case None    => 0
        case Some(l) => getParams(l).prelim + spacing(l,n,true)
      }
    } else if ( expanded ) {
      val leftMost = Proxy.getFirstChild(n)
      val rightMost = Proxy.getLastChild(n)
      var defaultAncestor = leftMost
      var c = leftMost
      
      @tailrec
      def firstWalk0(entry: Option[NodeItem], i: Int): Unit = entry match {
        case Some(c0) => 
          firstWalk(c0, i, depth+1)
          defaultAncestor = apportionVisible(c0, defaultAncestor.get)
          firstWalk0(Proxy.getNextSibling(c0), i + 1)
        case None     =>
      }
      firstWalk0(c, 0)
          
      executeShifts(n)
          
      val midpoint = 0.5 *
         (getParams(leftMost).prelim + getParams(rightMost).prelim)
          
      Proxy.getPreviousSibling(n) match {
        case Some(left) =>
          np.prelim = getParams(left).prelim + spacing(left, n, true)
          np.mod = np.prelim - midpoint
        case None       =>
          np.prelim = midpoint
      }
    }
  }
  
  override def apportion(v: NodeItem, a: NodeItem): NodeItem =
    throw new Exception("disallowed")
  
  def apportionVisible(v: NodeItem, a: NodeItem): Option[NodeItem] = {        
    Proxy.getPreviousSibling(v) match {
      case sibl@Some(w) =>
        var sip, sim, sop, som: Double = 0
  
        var vip: Option[NodeItem] = Some(v)
        var vop: Option[NodeItem] = Some(v)
        var vim: Option[NodeItem] = sibl
        var vom = Proxy.getFirstChild(v.getParent().asInstanceOf[NodeItem])
  
        sip = getParams(vip.get).mod
        sop = getParams(vop.get).mod
        sim = getParams(vim.get).mod
        som = getParams(vom.get).mod
        
        @tailrec
        def apportion0(nr0: Option[NodeItem], nl0: Option[NodeItem]): (Option[NodeItem], Option[NodeItem]) = (nr0, nl0) match {
          case (Some(nr1), Some(nl1)) =>
            vim = nr0
            vip = nl0
            vom = nextVisibleLeft(vom)
            vop = nextVisibleRight(vop)
            getParams(vop).ancestor = v
            val shift: Double = (getParams(vim).prelim + sim) - 
              (getParams(vip).prelim + sip) + spacing(vim.get, vip.get, false)
            if ( shift > 0 ) {
              moveSubtree(ancestor(vim,v,a), v, shift)
              sip += shift
              sop += shift
            }
            sim += getParams(vim).mod
            sip += getParams(vip).mod
            som += getParams(vom).mod
            sop += getParams(vop).mod
            apportion0(nextVisibleRight(vim), nextVisibleLeft(vip))
          case res =>
            res
        }
        val (nr, nl) = apportion0(nextVisibleRight(vim), nextVisibleLeft(vip))
        
        (nr, nextVisibleRight(vop)) match {
          case (Some(nr1), None) =>
            val vopp = getParams(vop)
            vopp.thread = nr1
            vopp.mod += sim - sop
          case _ =>
            ()
        }
        (nl, nextVisibleLeft(vom)) match {
          case (Some(nl1), None) =>
            val vomp = getParams(vom)
            vomp.thread = nl1
            vomp.mod += sip - som
            Some(v)
          case _ =>
            Some(a)
        }
      case None =>
        assert(a != null)
        Some(a)
    }
  }
  
  override def nextLeft(n: NodeItem): NodeItem =
    throw new Exception("dissallowed")
  
  override def nextRight(n: NodeItem): NodeItem =
    throw new Exception("dissallowed")
  
  def nextVisibleLeft(n: Option[NodeItem]): Option[NodeItem] =
    nextVisible(n, Proxy.getFirstChild _)
  
  def nextVisibleRight(n: Option[NodeItem]): Option[NodeItem] =
    nextVisible(n, Proxy.getLastChild _)
    
  def nextVisible(n: Option[NodeItem], getChild: NodeItem => Option[NodeItem]): Option[NodeItem] = {
    def noChild(n0: NodeItem) = {
      val tmp1 = getParams(n0).thread
      if (tmp1 != null) // shouldn't this be a precondition?
        Some(tmp1)
      else
        None
    }

    n match {
      case Some(n1) =>
        if ( n1.isExpanded() && Proxy.isValid(n1))
          getChild(n1) match {
            case None => noChild(n1)
            case next => next
          }
        else noChild(n1)
      case _        =>
        throw new Exception("Invalid operation: nextVisible")
        n
    }
  }
  
  override def getParams(item: NodeItem): Params =
    if (item != null) getParams(Some(item)) else (throw new Exception("Invalid operation: getParams"))
    
  def getParams(item: Option[NodeItem]): Params = item match {
    case Some(item0) => super.getParams(item0)
    case None        => throw new Exception("Invalid operation")
  }
  
  override def executeShifts(n: NodeItem) {
    @tailrec
    def executeShifts0(c0: Option[NodeItem], shift: Double, change: Double): Unit = c0 match {
      case Some(c1) =>
        val cp = getParams(c1)
        cp.prelim += shift
        cp.mod += shift
        val change1 = change + cp.change
        executeShifts0(Proxy.getPreviousSibling(c1), cp.shift + change1, change1)
      case None     =>
    }
    executeShifts0(Proxy.getLastChild(n), 0, 0)
  }

  override def ancestor(vim0: NodeItem, v: NodeItem, a: NodeItem): NodeItem =
    throw new Exception("disallowed")
    
  def ancestor(vim0: Option[NodeItem], v: NodeItem, a: NodeItem): NodeItem = {
    val p:NodeItem = v.getParent().asInstanceOf[NodeItem]
    vim0 match {
      case Some(vim) =>
        val vimp = getParams(vim)
        if (vimp.ancestor.getParent() == p) vimp.ancestor else a
      case None       =>
        a
    }
  }
  
  
  override def secondWalk(n: NodeItem, p: NodeItem, m: Double, depth: Int) {
    val np = getParams(n)

    setBreadth(n, p, np.prelim + m)
    setDepth(n, p, m_depths(depth))
    
    @tailrec
    def secondWalk0(c0: Option[NodeItem], depth0: Int): Unit = c0 match {
      case Some(c1) =>
        secondWalk(c1, n, m + np.mod, depth0)
        secondWalk0(Proxy.getNextSibling(c1), depth0)
      case None     =>
    }
    if (n.isExpanded() && Proxy.isValid(n))
      secondWalk0(Proxy.getFirstChild(n), depth + 1)
    np.clear()
  }
  
  // Provide main custom operation for navigating between nodes, 
  // that takes into account visibility requirements of the type debugger
  object Proxy {
    def isValid(node: NodeItem): Boolean =
      node.isVisible && isCustomNodeVisible(node)
    
    def getChildCount(underlying: NodeItem): Int =
      underlying.children_[NodeItem].toList.filter(isValid).size
    
    
    private def getChild(initial: NodeItem, backtrack: NodeItem => Option[NodeItem]): Option[NodeItem] =
      if (initial != null) if (!isValid(initial)) backtrack(initial) else Some(initial)
      else None
        
    def getFirstChild(underlying: NodeItem) =
      getChild(underlying.getFirstChild().asInstanceOf[NodeItem], Proxy.getNextSibling _)
    
    def getLastChild(underlying: NodeItem) =
      getChild(underlying.getLastChild().asInstanceOf[NodeItem], Proxy.getPreviousSibling _)
    
    private def getSibling(underlying: NodeItem, which: NodeItem => NodeItem): Option[NodeItem] = {
      var found = which(underlying)
      while (found != null && !isValid(found))
        found = which(found)
      if (found == null) None else Some(found)
    }
    
    def getPreviousSibling(underlying: NodeItem) =
      getSibling(underlying, _.getPreviousSibling().asInstanceOf[NodeItem])
    
    def getNextSibling(underlying: NodeItem) =
      getSibling(underlying, _.getNextSibling().asInstanceOf[NodeItem])
  }
}