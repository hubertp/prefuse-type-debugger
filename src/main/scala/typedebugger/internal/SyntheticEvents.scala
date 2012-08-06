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
  
  case class GroupCheckBoundsOfTArgs() extends SyntheticEvent {
    def tag = "synthetic-grouping-of-check-targ-bounds"
  }
  
  case class GroupCheckConstrInstantiationsBounds() extends SyntheticEvent {
    def tag = "synthetic-grouping-of-checking-of-constraint-instantiations"
  }
  
  case class MethTypeArgsResTpeCompatibleWithPt() extends SyntheticEvent {
    def tag = "is-method-result-tpe-compatible-with-pt"
  }
  
  case class ExprTypeTpeCompatibleWithPt() extends SyntheticEvent {
    def tag = "is-expr-tpe-compatible-with-pt"
  }
  
  case class TryToSolveTVars() extends SyntheticEvent {
    def tag = "try-to-solve-tvars-given-constraints"
  }
}