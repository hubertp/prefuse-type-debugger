package scala.typedebugger
package processing

trait InferStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  
  trait InferEventsOps {
    import TVarSetInst._
    private val DEFAULT = ("(infer| not-implemented)", "(infer| not-implemented)")
    
    def explainInferEvent(ev: Event with InferEvent)(implicit time: Clock = ev.time): (String, String) = {
      ev match {
        case e:InferDone =>
          ("Finished inference", "")
          
        case e:InferInstanceDone =>
          ("Inferred concrete instance", 
           snapshotAnyString(e.tree))
           
        case e:MethodInfer =>
          DEFAULT

        case e:AdjustTypeArgs =>
          DEFAULT
          
        case e:CompatibleTypes =>
          ("Compatible types,\ntry to solve any type variables",
           "Compatible types \n" +
           "Found:    " + snapshotAnyString(e.found) + "\n" +
           "Required: " + snapshotAnyString(e.pt) + "\n" +
           "with type parameters " + e.tparams.map(snapshotAnyString).mkString(","))
           
        case e: InstantiateTypeVars =>
          // TODO: list constraints for each
          ("Instantiate type variables",
           "Instantiate type variables: " + e.tvars.map(tvar => {val tvar0 = TypeSnapshot(tvar); anyString(tvar0) + " => " + snapshotAnyString(tvar0.constr.inst)}).mkString("[", ",", "]"))
           
        case e: InstantiateTypeVar =>
          val tvar1 = TypeSnapshot(e.tvar)
          ("Instantiate type variable " + anyString(tvar1),
           "Instantiate type variable: " + anyString(tvar1) + " with constraint " + snapshotAnyString(tvar1.constr))
           
        case e: SolveSingleTVar =>
          ("Solve type variable",
           "Solve: " + snapshotAnyString(e.tvar) + "\n" +
           //"In " + e.tvar.
           (if (!e.variance) "contravariant position" else ""))
           
        case e: SetInstantiateTypeConstraint =>
          ("Set instantiation for type constraint\n=> " + tvarSetInstExpl(e.reason),
           "")
           
        case e: WildcardLenientTArg =>
          if (e.noInstance)
            ("No instance of a typevariable " + snapshotAnyString(e.tvar), "")
          else
            ("No type var constraint instance for " + snapshotAnyString(e.tvar), "")
            
        case e: IncompatibleResultAndPrototype =>
          ("Incompatible function result- and proto-type",
           "Incompatible types:\n" + snapshotAnyString(e.restpe) + "vs.\n" + snapshotAnyString(e.pt))
          
        case e: InstantiateGlbOrLub =>
          def instType = if (e.up) "greater lower bound (glb)" else "lower upper bound (lub)"
          def pos = if (e.up) "non-contravariant position" else "contravariant position"
          lazy val tvar1 = TypeSnapshot(e.tvar)
          lazy val bounds = if (e.up) tvar1.constr.hiBounds else tvar1.constr.loBounds
          def boundsString(bs: List[Type]) = 
            if (bs.isEmpty) "empty bounds" else bs.map(snapshotAnyString).mkString("[", ",", "]")
          ("Calculate " + instType + "\nbecause type parameter is in\n" + pos,
           "Calculate " + instType + " for " + boundsString(bounds))
           
        case e: InstantiateGlbOrLubDone =>
          val instType = if (e.up) "glb" else "lub"
          ("Calculated " + instType + " as \n" + safeTypePrint(e.tp, truncate=false),
           "")
           
        case e: AddBoundTypeVar =>
          val boundType = if (e.upperBound) "upper" else "lower"
          ("Register " + boundType + " bound\nof type " + snapshotAnyString(e.bound),
           "")

        case e:InferExprInstance =>
          val snapshotTree = treeAt(e.tree)
          ("Infer expression instance",
           "Infer expression instance for tree \n" +
           snapshotAnyString(snapshotTree) + "\n" +
//           " of type '" + snapshotAnyString(snapshotTree.tpe) + "' \n" +
           "with undetermined typeparams '" + e.tparams.map(snapshotAnyString).mkString(",") + "' \n" +
           "and expected type " + snapshotAnyString(e.pt))
    
        case e:InferMethodInstance =>
          val snapshotTree = treeAt(e.tree)
          ("Infer method instance \n using inferred argument types",
           "Infer method instance for expression \n " +
           anyString(snapshotTree) + "\n" +
           "of type " + snapshotAnyString(snapshotTree.tpe) + "\n" +
           "Given arguments with types are " + e.args.map(a => {val a0 = treeAt(a); snapshotAnyString(a0.tpe)}).mkString(",") + "\n" +
           "and the expected type " + snapshotAnyString(e.pt) + "\n" +
           "Try to find substitution for type-parameter(s) " + e.undetparams.map(snapshotAnyString).mkString(",") + "\n" +
           "so that the result type of the application is compatible with the expected type" + snapshotAnyString(e.pt))

        case e:InferredMethodInstance =>
          ("Inferrred method instance",
           "Inferred method instance \n" + 
           snapshotAnyString(e.tree) + "\n" +
           "in application of arguments: \n" +
           e.args.map(arg => {val arg0 = treeAt(arg); anyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")"))

        case e:NoInstanceForMethodTypeParameters =>
          ("Cannot infer method instance \n as there is no instance \n for type parameter(s)",
           "No instance exists for type parameters for \n" +
           snapshotAnyString(e.tree) + "\n" +
           "exist so that it can be applied to \n" +
           e.args.map(arg => {val arg0 = treeAt(arg); snapshotAnyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")") + "\n" +
           "because " + e.exMsg)
           
        case e:MethTypeArgsSolve =>
          ("MethTypeArgsDebug",
           "Meth type args is: " + e.tparams.map(snapshotAnyString).mkString(","))
           
        case e:SolvedTypes =>
          ("Solved Types",
           "Solved types: " + e.solvedtypes.map(snapshotAnyString).mkString(","))
           
        case e:InferMethodInstanceTypeError =>
          ("Cannot infer method instance \n as encountered type error",
           "Type error \n" + e.exMsg + "\n" +
           "when inferring methods instance for \n" + snapshotAnyString(e.tree) + "\n" +
           " and arguments: \n" +
           e.args.map(arg => {val arg0 = treeAt(arg); snapshotAnyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")"))

        case e:FailedTypeCompatibilityInInferExprTypeArgs =>
          ("Incompatible types failure",
           "Type " + snapshotAnyString(e.tpe) + "\n" +
           "Expected " + snapshotAnyString(e.pt))

        case e:SubstituteTParamsInfer =>
          ("Substitute type parameters",
           "Substitute " + e.tparams.map(snapshotAnyString) + " with " + e.targs.map(snapshotAnyString) +
             (if (e.adjust) " with " else " without ") + "type arguments adjustment")
             
        case e:PolyTypeInstantiationError =>
          ("Cannot instantiate polymorhpic type \n to the expected type",
           "Polymorphic expression \n " + snapshotAnyString(e.tree) + "\n" +
           "cannot be instantiated to the expected type.\n" +
           "Found:    " + snapshotAnyString(e.polytype) + "\n" +
           "Expected: " + snapshotAnyString(e.pt) + "\n" +
           "Undetermined type-parameters: " + e.tparams.map(snapshotAnyString).mkString(","))
           
        case e:TreeTypeSubstitution =>
          val tpSubst = (e.undet zip e.targs).map(subst => snapshotAnyString(subst._1) + " => " + snapshotAnyString(subst._2)).mkString("\n")
          ("Type substitution map \n for inferred instance",
           "Perform substitution to infer concrete instance:\n" + tpSubst + "\n" +
           "in " + snapshotAnyString(e.tree))
           
        case e:IsApplicableInfer =>
          ("Is function applicable to\n arguments and expected type", 
           "Is function applicable to argument types and expected type " + snapshotAnyString(e.pt))
           
        case e:IsApplicableFallbackInfer =>
          ("Fallback: Is function applicable to\n arguments and no expected type",
           "Is applicable check failed with expected type " + snapshotAnyString(e.pt) +
           " so try with no expected type")
           
        case e:AdjustedTypeArgs =>
          DEFAULT
          
        case e:InferMethodAlternative =>
          // TODO: remove once we apply snapshots automatically to tree (during its snapshot recovery)
          //val snapshotTree = treeAt(e.tree, e.time)
          
          ("Infer method \n with alternatives\n [" + (if (e.implicits) "with implicits" else "without implicits")+ "]",
           "In " + snapshotAnyString(e.tree) + " with overloaded type\n " +
           combinedSnapshotAnyString(e.tree)(_.tpe) + "\n" +
           "we infer the correct, single method from alternatives suitable for " + e.argsTpes.map(snapshotAnyString) +
           " with undetermined type parameters " + e.tparams +
           "\nand expected final type " + snapshotAnyString(e.pt))
           
        case e:NoBestAlternativeTryWithWildcard =>
          ("No single best implict was found.\n Try searching without expected type",
           "Couldn't find best alternative for " + snapshotAnyString(e.tree) + ".\n" + 
             (if (!e.competing.isEmpty) "Conflict in " + e.competing else "") +
              ", will attempt to infer best alternative without pt.\n" +
             (if (!e.applicable.isEmpty) "Some applicable implicits are: " + e.applicable else "")) 
             
        case e:AmbiguousAlternatives =>
          ("Ambiguous alternatives for method",
           snapshotAnyString(e.tree) + "\n has ambigous alternatives. All applicable alternatives are:\n" +
           e.applicable + " of which the conflicting ones are\n" + e.competing)
           
        case e:VerifyMethodAlternative =>
          ("Verify alternative",
           "Check if method alternative " + e.alternative + " of type " + combinedSnapshotAnyString(e.alternative)(_.tpe) +
           " is applicable for arguments' types " + e.argsTypes.map(snapshotAnyString) +
           "\nand conforms to the actual expected type " + snapshotAnyString(e.pt))
           
        case e:PossiblyValidAlternative =>
          val result = if (e.result) "Valid" else "Invalid"
          (result + " alternative",
          "Alternative " + snapshotAnyString(e.alternative) + " with type " +
          combinedSnapshotAnyString(e.alternative)(_.tpe) + " is " + result)
          
        case e:MethodTypeApplicableDebug =>
          DEFAULT
          
        case e:FastTrackMethodTypeApplicableDebug =>
          DEFAULT
    
        case e:PolyTypeApplicableDebug =>
          DEFAULT
    
        case e:TypesCompatibleDebug =>
          DEFAULT
          
        case e:InferExprAlternative =>
          val snapshotTree = treeAt(e.tree)
          ("Infer expression \n with alternatives \n[" + (if (e.implicits) "with implicits" else "without implicits")+ "]",
           "In " + anyString(snapshotTree) + " with overloaded type " + snapshotAnyString(snapshotTree.tpe) +
           " we infer the alternative for type " + snapshotAnyString(e.pt))
           
        case e:ImprovesAlternativesCheck =>
          ("Compare conflicting alternatives", 
           "Compare conflicting, both applicable alternatives\nfor " + snapshotAnyString(e.tree))
           
        case e:AlternativesCheck =>
          (if (e.check) "Alternative improved" else "Alternatives are comparable", "")

        case _ =>
          DEFAULT
      }
    }
    
    def explainLubGlbEvent(ev: Event with LubEvent)(implicit time: Clock = ev.time): (String, String) = {
      ev match {
        case e: CalcLub =>
          ("Calculate lub " + lubKindExpl1(e.kind),
           "Calculating lub for types: " + e.tps.map(snapshotAnyString(_)).mkString("[", ",", "]"))
        case e: CalcLubElimSubTypes =>
          ("Calculate lub after eliminating subtypes " + lubKindExpl2(e.kind),
           "Calculating lub for types: " + e.tps.map(snapshotAnyString(_)).mkString("[", ",", "]"))
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