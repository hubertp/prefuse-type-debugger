package scala.typedebugger
package ui

object Filtering extends Enumeration with ImplicitEvents {
  val Subtyping      = Value("subtyping")
  val SubCheck       = Value("subtype checks")
  val AltComp        = Value("compare alternatives")
  val QuickAltFilter = Value("quick alternatives filtering")
  val TypesComp      = Value("types compatibility")
  val ProtoTpeArgs   = Value("inferred prototype arguments")
  val ValidateParent = Value("validate parent class (scala.ScalaObject or java.lang.Object)")
  val ConvConstr     = Value("convert constructor body")
  
  val TemplateSynth  = Value("synthetic template")
  val DefSynth       = Value("synthetic definition")
  val Constr         = Value("constructor")
}

trait ImplicitEvents extends Enumeration {
  self: Filtering.type =>
  
  val ImplElig       = Value("implicits eligibility")
  val VerifyImpl     = Value("verify implicit")
}