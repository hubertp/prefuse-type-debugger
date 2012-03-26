package scala.typedebugger
package internal

import scala.tools.nsc.Global

trait DebuggerPositions {
  self: Global =>
    
  // Locate the smallest statement that encloses `pos`
  class StatementLocator(pos: Position) extends Traverser {
    var last: Tree = _
    def locateStat(root: Tree): Tree = {
      last = EmptyTree
      traverse(root)
      last
    }
    
    override def traverseStats(stats: List[Tree], exprOwner: Symbol) {
      var located = false // safeguard, is it really necessary if positions are ok?
      stats foreach (stat => 
        if (!located && (stat.pos includes pos)) {
          if (!stat.pos.isTransparent) {
            assert(stat.tpe != null)
            last = stat
          }
          super.traverse(stat)
          located = true
        }
      )
    }
  }

}