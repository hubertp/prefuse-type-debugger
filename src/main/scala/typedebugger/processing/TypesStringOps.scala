package scala.typedebugger
package processing

trait TypesStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
    
  trait TypesEventsOps extends AnyRef with SubtypingInfo {
    private val DEFAULT = ("(types| not-implemented)", "(types| not-implemented)")
    
    def explainTypesEvent(ev: Event with TypesEvent)(implicit time: Clock = ev.time) = ev match {
      case e: SubTypeCheck =>
        ("Subtyping check" + truncateStringRep(safeTypePrint(e.lhs, truncate=false),safeTypePrint(e.rhs, truncate=false), " <: ", "\n"),
         "Subtype check for\n" + snapshotAnyString(e.lhs) + " <: " + snapshotAnyString(e.rhs))

      case e: SubTypeCheckRes =>
        (if (e.res) "Succeeded" else "Failed", "")
        
      case e: SubTypeCheckArg =>
        val varianceInfo =
          if (e.variance > 0) "covariant"
          else if (e.variance < 0) "contravariant"
          else "invariant"
        ("Compare type arguments\n in the " + varianceInfo + " position", "")

      case e: CompareTypes =>
        (explainSubtyping(e.compType, e.which),
         "Subtyping check for:\n " + snapshotAnyString(TypeSnapshot(e.tp1)) + " <:< " + snapshotAnyString(TypeSnapshot(e.tp2)))
      
      case e: CompareDone =>
        (if (e.subtypes) "Succeeded" else "Failed", "")
        
      case e: FailedSubtyping =>
        ("Types are not subtypes", snapshotAnyString(e.tp1) + " <:/< " + snapshotAnyString(e.tp2))
        
      case e: InstantiateTypeParams =>
        ("Instantiate type parameters",
         "Instantiating: " + e.formals.zip(e.actuals).map(subst => snapshotAnyString(subst._1) + " ==> " + snapshotAnyString(subst._2)).mkString("\n"))

      case _ => DEFAULT
      
    }
  }
  
  trait SubtypingInfo {
    import SubCompare._
    import Side._
    
    def explainSubtyping(kind: SubCompare.Value, which: Side.Value): String = kind match {
      case CTypeRef if which == Both =>
        "Subtyping check between type references"
      case CTypeRef =>
        "Subtyping check with type reference" + explainSide(which)
      case CAnnotated =>
        "Subtyping check with annotated type" + explainSide(which)
      case CSingletonClass =>
        "Subtyping check involving Singleton class" + explainSide(which)
      case CClassSym =>
        "Subtyping check involving Class symbol" + explainSide(which)
      case CSkolemizedExist =>
        "Subtyping check involivng existentials" + explainSide(which)
      case CRefined =>
        "Subtyping check with refined type" + explainSide(which)
      case CNullary =>
        "Subtyping check with nullary method type" + explainSide(which)
      case CTypeBounds =>
        "Subtyping check between type bounds"
      case CMethod =>
        "Subtyping check between method types"
      case COther =>
        ""
      case _ =>
        ""
    }
    
    def explainSide(which: Side.Value): String = which match {
      case Left  => " as a subtype" 
      case Right => " as a supertype"
      case Both  => ""
      case Other => ""
    }
    
    
  }
}