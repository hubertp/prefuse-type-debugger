package scala.typedebugger
package processing

trait InferStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  
  trait InferEventsOps {
    import TVarSetInst._
    private val DEFAULT = ("(infer| not-implemented)", "(infer| not-implemented)")
    
    def explainInferEvent(ev: Event with InferEvent): (String, String) = {
      ev match {
        case e:InferDone =>
          ("Finished inference", "")
          
        case e:InferInstanceDone =>
          ("Inferred concrete instance", 
           anyString(e.tree))
           
        case e:MethodInfer =>
          DEFAULT

        case e:AdjustTypeArgs =>
          DEFAULT
          
        case e:CompatibleTypes =>
          ("Compatible types,\ntry to solve any type variables",
           "Compatible types \n" +
           "Found:    " + anyString(e.found) + "\n" +
           "Required: " + anyString(e.pt) + "\n" +
           "with type parameters " + e.tparams.map(anyString).mkString(","))
           
        case e: InstantiateTypeVars =>
          // TODO: list constraints for each
          ("Instantiate type variables",
           "Instantiate type variables: " + e.tvars.map(tvar => anyString(tvar) + " => " + anyString(tvar.constr.inst)).mkString("[", ",", "]"))
           
        case e: InstantiateTypeVar =>
          ("Instantiate type variable " + e.tvar,
           "Instantiate type variable: " + anyString(e.tvar) + " with constraint " + anyString(e.tvar.constr))
           
        case e: SolveSingleTVar =>
          ("Solve type variable",
           "Solve: " + e.tvar + "\n" +
           //"In " + e.tvar.
           (if (!e.variance) "contravariant position" else ""))
           
        case e: SetInstantiateTypeConstraint =>
          ("Set instantiation for type constraint\n=> " + tvarSetInstExpl(e.reason),
           "")
           
        case e: WildcardLenientTArg =>
          if (e.noInstance)
            ("No instance of a typevariable " + e.tvar, "")
          else
            ("No type var constraint instance for " + e.tvar, "")
            
        case e: IncompatibleResultAndPrototype =>
          ("Incompatible function result- and proto-type",
           "Incompatible types:\n" + anyString(e.restpe) + "vs.\n" + anyString(e.pt))
          
        case e: InstantiateGlbOrLub =>
          def instType = if (e.up) "greater lower bound (glb)" else "lower upper bound (lub)"
          def pos = if (e.up) "non-contravariant position" else "contravariant position"
          lazy val bounds = if (e.up) e.tvar.constr.hiBounds else e.tvar.constr.loBounds
          def boundsString(bs: List[Type]) = 
            if (bs.isEmpty) "empty bounds" else bs.map(anyString).mkString("[", ",", "]")
          ("Calculate " + instType + "\nbecause type parameter is in\n" + pos,
           "Calculate " + instType + " for " + boundsString(bounds))
           
        case e: InstantiateGlbOrLubDone =>
          val instType = if (e.up) "glb" else "lub"
          ("Calculated " + instType + " as \n" + safeTypePrint(e.tp, truncate=false),
           "")
           
        case e: AddBoundTypeVar =>
          val boundType = if (e.upperBound) "upper" else "lower"
          ("Register " + boundType + " bound\nof type " + anyString(e.bound),
           "")

        case e:InferExprInstance =>
          ("Infer expression instance",
           "Infer expression instance for tree \n" +
           anyString(e.tree) + "\n" +
           " of type '" + anyString(e.tree) + "' \n" +
           "with undetermined typeparams '" + e.tparams.map(anyString).mkString(",") + "' \n" +
           "and expected type " + anyString(e.pt))
    
        case e:InferMethodInstance =>
          ("Infer method instance \n using inferred argument types",
           "Infer method instance for expression \n " +
           anyString(e.tree) + "\n" +
           "of type " + anyString(e.tree.tpe) + "\n" +
           "Given arguments with types are " + e.args.map(a => anyString(a.tpe)).mkString(",") + "\n" +
           "and the expected type " + anyString(e.pt) +
           "Try to find substitution for type-parameter(s) " + e.undetparams.map(anyString).mkString(",") + "\n" +
           "so that the result type of the application is compatible with the expected type" + anyString(e.pt))

        case e:InferredMethodInstance =>
          ("Inferrred method instance",
           "Inferred method instance \n" + 
           anyString(e.tree) + "\n" +
           "in application of arguments: \n" +
           e.args.map(arg => anyString(arg) + ":" + anyString(arg.tpe)).mkString("(", ",", ")"))

        case e:NoInstanceForMethodTypeParameters =>
          ("Cannot infer method instance \n as there is no instance \n for type parameter(s)",
           "No instance exists for type parameters for \n" +
           anyString(e.tree) + "\n" +
           "exist so that it can be applied to arguments: \n" +
           e.args.map(arg => anyString(arg) + ":" + anyString(arg.tpe)).mkString("(", ",", ")") + "\n" +
           "because " + e.exMsg)
           
        case e:MethTypeArgsSolve =>
          ("MethTypeArgsDebug",
           "Meth type args is: " + e.tparams.map(anyString).mkString(","))
           
        case e:SolvedTypes =>
          ("Solved Types",
           "Solved types: " + e.solvedtypes.map(anyString).mkString(","))
           
        case e:InferMethodInstanceTypeError =>
          ("Cannot infer method instance \n as encountered type error",
           "Type error \n" + e.exMsg + "\n" +
           "when inferring methods instance for \n" + anyString(e.tree) + "\n" +
           " and arguments: \n" +
           e.args.map(arg => anyString(arg) + ":" + anyString(arg.tpe)).mkString("(", ",", ")"))

        case e:FailedTypeCompatibilityInInferExprTypeArgs =>
          ("Incompatible types failure",
           "Type " + anyString(e.tpe) + "\n" +
           "Expected " + anyString(e.pt))

        case e:SubstituteTParamsInfer =>
          ("Substitute type parameters",
           "Substitute " + e.tparams.map(anyString) + " with " + e.targs.map(anyString) +
             (if (e.adjust) " with " else " without ") + "type arguments adjustment")
             
        case e:PolyTypeInstantiationError =>
          ("Cannot instantiate polymorhpic type \n to the expected type",
           "Polymorphic expression \n " + anyString(e.tree) + "\n" +
           "cannot be instantiated to the expected type.\n" +
           "Found:    " + anyString(e.polytype) + "\n" +
           "Expected: " + anyString(e.pt) + "\n" +
           "Undetermined type-parameters: " + e.tparams.map(anyString).mkString(","))
           
        case e:TreeTypeSubstitution =>
          val tpSubst = (e.undet zip e.targs).map(subst => subst._1 + " => " + subst._2).mkString("\n")
          ("Type substitution map \n for inferred instance",
           "Perform substitution to infer concrete instance:\n" + tpSubst + "\n" +
           "in " + e.tree)
           
        case e:IsApplicableInfer =>
          ("Is function applicable to\n arguments and expected type", 
           "Is function applicable to argument types and expected type " + anyString(e.pt))
           
        case e:IsApplicableFallbackInfer =>
          ("Fallback: Is function applicable to\n arguments and no expected type",
           "Is applicable check failed with expected type " + anyString(e.pt) +
           " so try with no expected type")
           
        case e:AdjustedTypeArgs =>
          DEFAULT
          
        case e:InferMethodAlternative =>
          ("Infer method \n with alternatives\n [" + (if (e.implicits) "with implicits" else "without implicits")+ "]",
           "In " + anyString(e.tree) + " with overloaded type\n " +
           anyString(e.tree.tpe) + "\n" +
           "we infer the correct, single method from alternatives suitable for " + e.argsTpes.map(anyString) +
           " with undetermined type parameters " + e.tparams +
           "\nand expected final type " + anyString(e.pt))
           
        case e:NoBestAlternativeTryWithWildcard =>
          ("No single best implict was found.\n Try searching without expected type",
           "Couldn't find best alternative for " + anyString(e.tree) + ".\n" + 
             (if (!e.competing.isEmpty) "Conflict in " + e.competing else "") +
              ", will attempt to infer best alternative without pt.\n" +
             (if (!e.applicable.isEmpty) "Some applicable implicits are: " + e.applicable else "")) 
             
        case e:AmbiguousAlternatives =>
          ("Ambiguous alternatives for method",
           anyString(e.tree) + "\n has ambigous alternatives. All applicable alternatives are:\n" +
           e.applicable + " of which the conflicting ones are\n" + e.competing)
           
        case e:VerifyMethodAlternative =>
          ("Verify alternative",
           "Check if method alternative " + e.alternative + " of type " + anyString(e.alternative.tpe) +
           " is applicable for arguments' types " + e.argsTypes.map(anyString) +
           "\nand conforms to the actual expected type " + anyString(e.pt))
           
        case e:PossiblyValidAlternative =>
          val result = if (e.result) "Valid" else "Invalid"
          (result + " alternative",
          "Alternative " + anyString(e.alternative) + " with type " +
          anyString(e.alternative.tpe) + " is " + result)
          
        case e:MethodTypeApplicableDebug =>
          DEFAULT
          
        case e:FastTrackMethodTypeApplicableDebug =>
          DEFAULT
    
        case e:PolyTypeApplicableDebug =>
          DEFAULT
    
        case e:TypesCompatibleDebug =>
          DEFAULT
          
        case e:InferExprAlternative =>
          ("Infer expression \n with alternatives \n[" + (if (e.implicits) "with implicits" else "without implicits")+ "]",
           "In " + anyString(e.tree) + " with overloaded type " + anyString(e.tree.tpe) +
           " we infer the alternative for type " + anyString(e.pt))
           
        case e:ImprovesAlternativesCheck =>
          ("Compare conflicting alternatives", 
           "Compare conflicting, both applicable alternatives\nfor " + anyString(e.tree))
           
        case e:AlternativesCheck =>
          (if (e.check) "Alternative improved" else "Alternatives are comparable", "")

        case _ =>
          DEFAULT
      }
    }
    
    def explainLubGlbEvent(ev: Event with LubEvent): (String, String) = {
      ev match {
        case e: CalcLub =>
          ("Calculate lub " + lubKindExpl1(e.kind),
           "Calculating lub for types: " + e.tps.map(anyString).mkString("[", ",", "]"))
        case e: CalcLubElimSubTypes =>
          ("Calculate lub after eliminating subtypes " + lubKindExpl2(e.kind),
           "Calculating lub for types: " + e.tps.map(anyString).mkString("[", ",", "]"))
        case _ =>
          DEFAULT
      }
    }
    
    def lubKindExpl1(v: LubKindEntry.Value): String = {
      import LubKindEntry._
      v match {
        case Empty =>
          "\nsolution: Maximize lub for empty bound"
        case SingleElem =>
          "\nsolution: Lub for a single type bound"
        case NonTrivial =>
          ""
      }      
    }
    
    
    def lubKindExpl2(v: LubKindElimSubtypes.Value): String = {
      import LubKindElimSubtypes._
      v match {
        case Empty =>
          "\nsolution: Maximize lub for empty bound"
        case SingleElem =>
          "\nsolution: Lub for a single type bound"
        case PolyTpe =>
          "for Polymorphic type"
        case MethodTpe =>
          "for Method type"
        case NullaryMethodTpe =>
          "for Nullary method type"
        case TpeBounds =>
          "for Type bounds"
        case NonTrivial =>
          "\n solution: Refined type for lub"
      }      
    }
    
    def tvarSetInstExpl(v: TVarSetInst.Value): String = v match {
      case ValidType =>
        ""
      case CovariantPos =>
        "Type variable in covariant position with non-empty list of upper bounds"
      case ContravariantPos =>
        "Type variable in contravariant position with non-empty list of lower bounds"
      case UpperSubLower =>
        "Upper bound is a subtype of a lower bound"
      case Solve =>
        "Solution dependent upon the co/contra/in-variant positon"
      case Relatable =>
        "Register type equality"
    }
  }
}