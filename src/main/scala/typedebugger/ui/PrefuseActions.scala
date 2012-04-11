package scala.typedebugger
package ui

// Instead of using plain strings use at least enumeration values for controlling the actions
object PrefuseActions extends Enumeration {
  val textColor       = IndexedValue("textColor")
  val repaint         = IndexedValue("repaint")
  val fullPaint       = IndexedValue("fullPaint")
  val animatePaint    = IndexedValue("animatePaint")
  val treeLayout      = IndexedValue("treeLayout")
  val subLayout       = IndexedValue("subLayout")
  val filter          = IndexedValue("filter")
  val animate         = IndexedValue("animate")
  val orient          = IndexedValue("orient")
  val initialGoals    = IndexedValue("initialGoals")
  val advancedOptions = IndexedValue("advancedOptions")
  val hideAll         = IndexedValue("hideAll")
      
  class IndexAwareAction(i: Int, name: String) extends Val(i, name) {
    def idxAction(implicit idx: Int) = name + idx
  }
  
  def IndexedValue(name: String) = new IndexAwareAction(nextId, name)
}