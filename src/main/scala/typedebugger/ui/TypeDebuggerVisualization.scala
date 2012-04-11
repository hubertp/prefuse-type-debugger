package scala.typedebugger
package ui

import prefuse.Visualization
import prefuse.action.Action
import prefuse.activity.Activity
import prefuse.controls.FocusControl

import PrefuseActions.IndexAwareAction

class TypeDebuggerVisualization() extends Visualization() {
  override def putAction(name: String, action: Action): Action = {
    throw new Exception("Type Debugger bug! Calling old putAction()")
  }
  
  def putAction(name: IndexAwareAction, action: Action)(implicit idx: Int): Action = {
    super.putAction(name.idxAction, action)
  }
  
  def run(name: IndexAwareAction)(implicit idx: Int):Activity = {
    super.run(name.idxAction)
  }
  
  override def cancel(name: String):Activity = {
    throw new Exception("Type Debugger bug! Calling old cancel()")
  }
  
  def cancel(name: IndexAwareAction)(implicit idx: Int):Activity = {
    super.cancel(name.idxAction)
  }
  
  override def getAction(name: String): Action = {
    throw new Exception("Type Debugger bug! Calling old getAction()")
  }
  
  def getAction(name: IndexAwareAction)(implicit idx: Int): Action = {
    super.getAction(name.idxAction)
  }

  override def alwaysRunAfter(name1: String, name2: String):Activity = {
    throw new Exception("Type Debugger bug! Calling old alwaysRunAfter()")
  }
  
  def alwaysRunAfter(name1: IndexAwareAction, name2: IndexAwareAction)(implicit idx: Int):Activity = {
    super.alwaysRunAfter(name1.idxAction, name2.idxAction)
  }
}

object TypeDebuggerFocusControl {
  def apply(i: Int, action: IndexAwareAction)(implicit idx: Int) = new FocusControl(i, action.idxAction)
}