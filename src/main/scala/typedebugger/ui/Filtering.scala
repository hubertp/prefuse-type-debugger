package scala.typedebugger
package ui

object Groups extends Enumeration {
  type Group = Value
  implicit val Implicits = Value
  implicit val Synthetics = Value
  implicit val NoGroup = Value("General")
}

import Groups.Group

object Filtering extends Enumeration with ImplicitFiltering with SyntheticFiltering {
  class GroupVal(i: Int, name: String, parent: Group) extends Val(i, name) {
    def group: Group = parent
  }
  
  protected def GroupValue(name: String)(implicit parent: Group) =
    new GroupVal(nextId, name, parent)
  
  val Subtyping      = Value("subtyping")
  val SubCheck       = Value("subtype checks")
  val AltComp        = Value("compare alternatives")
//  val QuickAltFilter = Value("quick alternatives filtering")
  val TypesComp      = Value("types compatibility")
  val ProtoTpeArgs   = Value("inferred prototype arguments")
  val ValidateParent = Value("validate parent class (scala.ScalaObject or java.lang.Object)")
  val ConvConstr     = Value("convert constructor body")
  
  // hide under 'Types' section
  val IsWithinBounds = Value("is instantiation of type variable constraint within bounds")
  val GroupIsWithinBounds = Value("are instantiations of type variable constraints within bounds")
}

trait ImplicitFiltering {
  self: Filtering.type =>

  import Groups.Implicits

  val ImplElig       = GroupValue("implicits eligibility")
  val VerifyImpl     = GroupValue("verify implicit")
}

trait SyntheticFiltering {
  self: Filtering.type =>
  
  import Groups.Synthetics
  
  val TemplateSynth  = GroupValue("synthetic template")
  val DefSynth       = GroupValue("synthetic definition")
  val Constr         = GroupValue("constructor")
}