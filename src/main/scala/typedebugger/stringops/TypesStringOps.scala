package scala.typedebugger
package stringops

trait TypesStringOps {
  self: StringOps with internal.CompilerInfo with internal.PrefuseStructure =>
    
  import global._
  import EV._
  import util.StringFormatter._
    
  trait TypesEventsOps extends AnyRef with SubtypingInfo {
    self: Descriptors =>
    private val DEFAULT = new DefaultDescriptor("types")
    
    def explainTypesEvent(ev: Event with TypesEvent, uiNode: UINode[PrefuseEventNode])(implicit time: Clock = ev.time) = ev match {
      case e: SubTypeCheck    =>
        new Descriptor {
          private def defaultMsg = "Is " + safeTypePrint(e.lhs, slice=true) + "\na subtype of\n " + safeTypePrint(e.rhs, slice=true) + "?"
          def basicInfo = uiNode.parent match {
            case Some(p) =>
              p.ev match {
                case AdaptStart(_, _)    =>
                  p.children.indexOf(uiNode) match {
                    case 0 =>
                      "Is " + safeTypePrint(e.lhs, slice=true) + "\na subtype of\n " + safeTypePrint(e.rhs, slice=true) + "\nin order to determine if adaptation is needed?"
                    case 1 =>
                      "Does folding the constants allows us to\nsatisfy the subtyping constraint involving expected type?"
                    case _ =>
                      defaultMsg
                  }
                case InfoEligibleTest(_)          =>
                  "Is the type of the implicit a subtype of the one we are after?"
                case InferExprInstance(_, _, _)   =>
                  "Is the current expression type template " + safeTypePrint(e.lhs, slice=true) +
                  "\n a subtype of\n " + safeTypePrint(e.rhs, slice=true) + "?"
                case InferMethodInstance(_, _, _, _) =>
                  "Is the current function result type template " + safeTypePrint(e.lhs, slice=true) +
                  "\na subtype of expected type\n " + safeTypePrint(e.rhs, slice=true) + "?"
                case _: ProtoTypeArgsDoTypedApply =>
                  "Is the result type of function compatible with expected type\n(collect local constraints when possible)?"
                case _                            =>
                  // more specialised error message
                  if (e.lhs.isHigherKinded || e.rhs.isHigherKinded)
                    "Can the subtyping check involving higher-kinded type(s) succeed?"
                  else
                    defaultMsg
              }
            case None    =>
              // crash
              throw new Exception("Invalid tree structure")
          }
          def fullInfo  = "Subtype check between %tpe and %tpe".dFormat(Some("Subtype check"),
              snapshotAnyString(e.lhs), snapshotAnyString(e.rhs)) 
        }

      case e: SubTypeCheckRes =>
        new Descriptor {
          def basicInfo = if (e.res) "Succeeded" else "Failed"
          def fullInfo  = ""
        }
        
      // todo: explain more how the number of subtype checks depends on the variance
      case e: SubTypeCheckArg =>

        new Descriptor {
          def basicInfo = {
            val varianceInfo =
              if (e.variance > 0) "covariant"
              else if (e.variance < 0) "contravariant"
              else "invariant"
            "Are the two type arguments\nin " + varianceInfo + " position comparable?"
          } 
          def fullInfo  = ""
        }
        
      // todo: should display bounds
      case e: IsWithinBounds  =>
        new Descriptor {
          def basicInfo = "Is the inferred instantiation of the constraint\nwithin the constraints bounds?"
          def fullInfo  = "Is constraint instantiation %tpe within its constraint bounds?".dFormat(Some("Bounds checking"),
            snapshotAnyString(e.tp))
        }
        
      case e: IsTArgWithinBounds =>
        new Descriptor {
          def basicInfo = "Is the inferred type argument within bounds\nof the formal type parameter?"
          def fullInfo  = "Does type argument %tpe correspond to bounds %tpe?".dFormat(
              Some("Type argument vs. type parameter bounds checking"),
              snapshotAnyString(e.targ),
              snapshotAnyString(e.bounds))
        }
      
      case TArgWithinBoundsDone(_, res) =>
        new Descriptor {
          def basicInfo = "Type argument is " + (if (res) "" else "not ") + "within formal type parameter bounds"
          def fullInfo  = ""
        }
        
      case RegisterBound(tvar, bound, _) =>
        new Descriptor {
          def basicInfo = "Registering bound"
          def fullInfo  = "Register bound %tpe for %tpe".dFormat(snapshotAnyString(tvar), snapshotAnyString(bound))
        }

      case e: CompareTypes =>
        new Descriptor {
          def basicInfo = explainSubtyping(e.compType, e.which, e.tp1, e.tp2)
          def fullInfo  = "Subtyping constraint test for %tpe <:< %tpe".dFormat(Some("Subtyping constraint check"),
              snapshotAnyString(TypeSnapshot.mapOver(e.tp1)), snapshotAnyString(TypeSnapshot.mapOver(e.tp2)))
        }
      
      case e: CompareDone =>
        new Descriptor {
          def basicInfo = if (e.subtypes) "Succeeded" else "Failed"
          def fullInfo  = ""
        }
        
      case FailedSubtyping(tp1, tp2, _, comp) =>
        new Descriptor {
          def basicInfo = comp match {
              case SubCompare.CHigherKindedTpe =>
                "Subtyping check infolving higher-kinded type failed"
              case _                =>
                "Types are not subtypes"
            }
          def fullInfo  = "Failed subtyping constraint for %tpe <:/< %tpe".dFormat(Some("Failed subtyping constraint"),
              snapshotAnyString(tp1), snapshotAnyString(tp2))
        }
        
      case e: InstantiateTypeParams =>
        new Descriptor {
          def basicInfo = "Instantiate type parameters"
          def fullInfo  = "Instantiating: %tpe".dFormat(e.formals.zip(e.actuals).map(subst => snapshotAnyString(subst._1) + " ==> " + snapshotAnyString(subst._2)).mkString("\n"))
        }

      case _ => DEFAULT
      
    }
  }
  
  trait SubtypingInfo {
    import SubCompare._
    import Side._

    def explainSubtyping(kind: SubCompare.Value, which: Side.Value, tp1: Type, tp2: Type)(implicit time: Clock) = kind match {
      case CTypeRef if which == Both =>
        "Are two named types\n(type references) subtypes?"
      case CTypeRef =>
        "Subtyping check with type reference" + explainSide(which)
      case CTypeRefBase =>
        // TODO: better explanation
        "Does converting " + explainSideSimplified(which) + " to its base type\n" +
        "make the subtyping constraint succeed?"
      case CAnnotated =>
        "Are the annotated types subtypes?"
      case CSingletonClass =>
        select(which){
          "Subtyping check involving Singleton class" + explainSide(which)
        }{
          "Does this type denote a stable reference?"
        }
      case CClassSym => // no longer used
        "Subtyping check involving Class symbol" + explainSide(which)
      case CSkolemizedExist =>
        "Is skolemized existential type " + safeTypePrint(select(which)(tp1)(tp2), slice=true) + " " +
        select(which)("a subtype")("a supertype") + " of the other type?"
      case CRefined =>
        "Is refined type " + safeTypePrint(select(which)(tp1)(tp2), slice=true) + " " +
        select(which)("a subtype")("a supertype") + " of the other type?" 
      case CNullary =>  // ignore?
        "Subtyping check with nullary method type" + explainSide(which)
      case CTypeBounds =>
        "Are the two type bounds subtypes?"
      case CMethod =>
        "Are two Method Types subtypes\n(including parameters and result types)?"
      case CClassSymRaw =>
        "Does mapping " + explainSideSimplified(which) + " raw type to an existential type\n" +
        "make the subtyping constraint succeed?"
      case CClassSymRefined if which == Right =>
        "Is " + safeTypePrint(tp1, slice=true) + " a subtype of refined type " + safeTypePrint(tp2, slice=true) + "?"
      case CClassSymRefined =>
        "Is refined type " + safeTypePrint(tp1, slice=true) + " a subtype of " + safeTypePrint(tp2, slice=true) + "?"
      case CTypeSymDeferred =>
        val msg = select(which)("upper")("lower")
        "Do " + msg + " bounds of the abstract type member (" + explainSideSimplified(which) + ")\n" +
        "make the subtyping constraint succeed?"
      case CTypeSymNonDeferred =>
        val msg = select(which)(tp1)(tp2) match {
          case TypeRef(_, sym:ModuleClassSymbol, _) =>
            "module class type"
          case TypeRef(_, sym:TypeSymbol, _) =>
            if (sym.isAliasType) "alias type"
            else if (sym.isSkolem) "type skolem"
            else "class type"
          case _                             =>
            "type"                           // fallback, should happen
        }
        "Does normalizing the " + msg + " (currenlty " + explainSideSimplified(which) + ")\n" +
        "make the subtyping constraint succeed?"
      
      case CSingletonOrNotNull => // todo: could be ignored?
        val msg = select(which)(tp1)(tp2) match {
          case _: SingletonType => "with Singleton Type mixed-in"
          case _: NotNullType   => "with NotNull Type mixed-in"
          case _                => "" // invalid
        }
        "Subtyping check involving type " + msg + explainSide(which)
      case CHigherKindedParams =>
        "Are corresponding type constructor parameters subtypes?"
      case CHigherKindedRes    =>
        "Are the two underlying type constructors subtypes?"
      case COther =>
        "Unknown kind of subtyping check"
      case _ =>
        "Unknown kind of subtyping check: " + kind
    }
    
    def select[T](which: Side.Value)(choiceL: => T)(choiceR: => T): T = which match {
      case Left  => choiceL
      case Right => choiceR
      case _     => choiceL // don't care?
    }
    
    def explainSide(which: Side.Value): String = 
      select(which)(" as a subtype")(" as a supertype")
    
    def explainSideSimplified(which: Side.Value): String =
      select(which)("subtype")("supertype")    
  }
}