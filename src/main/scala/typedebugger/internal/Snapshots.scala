package scala.typedebugger
package internal

trait Snapshots { self: scala.reflect.internal.SymbolTable =>
  
  def atClock(tree: Tree, clock: Clock): Tree = {
    new TreeSnapshot(clock).transform(tree) 
  }
  
  class TreeSnapshot(time: Clock) extends Transformer {
    lazy val strictCopier = newStrictTreeCopier
    
    def snapshot(tree: Tree): Option[AttributesHistory] = {
      if (tree != null) {
        val attrs = tree.attributes(time)
        val currentAttrs = tree.currentAttributes()
      
        if (attrs == currentAttrs) None else Some(attrs)
      } else None
    }
    
    override def transform(tree: Tree): Tree = {
      val t1 = super.transform(tree)
      
      val t = snapshot(tree) match {
        case None        => t1
        case Some(attrs) =>
          // strict copy the tree (outer layer only)
          val t2 = t1.shallowDuplicate
          t2.setTypeNoLog(attrs.tpe)
          if (tree.hasSymbol) t2.setSymbolNoLog(attrs.sym)
          t2
      }
      t setPos makeTransparent(tree.pos)
      t 
    }
  }

}