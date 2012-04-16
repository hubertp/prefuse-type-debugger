package scala.typedebugger
package internal



trait SyntheticEvents {
  self: CompilerInfo =>
    
  import global._
  import EV._
  
  sealed abstract class SyntheticEvent extends Event {
    override val phase = scala.tools.nsc.NoPhase
    override val unit = NoCompilationUnit
    override val file = None
    override def defaultPos = NoPosition
    override def pos = NoPosition
    def participants = List()
  }
  
  case object DummyRootEvent extends SyntheticEvent {
    def tag = "dummy-root"
  }
  
  case class GroupEligibleImplicits(source: ImplicitInfoSource) extends SyntheticEvent {
    def tag = "synthetic-grouping-of-implicits"
  }
}