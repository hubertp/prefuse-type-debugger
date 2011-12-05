package scala.typedebugger
package internal

import scala.collection.mutable.ListBuffer

trait IStructure {
  self: CompilerInfo =>

  class EventNode(val ev: global.EV.Event, val evs: ListBuffer[EventNode],
                  val parentENode: Option[EventNode])
}