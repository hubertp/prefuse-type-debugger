package scala.typedebugger
package stringops

trait InferStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  import util.StringFormatter._
  
  trait InferEventsOps {
    self: Descriptors =>
      
    import TVarSetInst._
    private val DEFAULT = new DefaultDescriptor("infer")
    
    def explainInferEvent(ev: Event with InferEvent)(implicit time: Clock = ev.time): Descriptor = {
      ev match {
        case e:InferDone =>
          new Descriptor {
            def basicInfo = "Finished inference"
            def fullInfo  = ""
          }
          
        case e:InferInstanceDone =>
          new Descriptor {
            def basicInfo = "Inferred concrete instance"
            def fullInfo  = {
              val tree1 = treeAt(e.tree)
              ("Inferred %tree as %tpe.\n" +
              "Still undetermined type parameters: %tpe").dFormat(Some("Inferred concrete instance"),
              anyString(tree1), snapshotAnyString(tree1.tpe), e.stillUndet.map(snapshotAnyString(_)).mkString("[", ",", "]"))
            }
          }
           
        case e:MethodInfer =>
          DEFAULT

        case e:AdjustTypeArgs =>
          DEFAULT
          
        case e:CompatibleTypes =>
          new Descriptor {
            def basicInfo = "Current expression type and the expected type are compatible.\n Try to solve any type constraints."
            def fullInfo  =
              ("Compatible types.\n" +
              "Found:    %tpe\n" +
              "Required: %tpe\n" +
              "with type parameters %tpe").dFormat(Some("Solve any type constraints for compatible types"), 
              snapshotAnyString(e.found), snapshotAnyString(e.pt), e.tparams.map(snapshotAnyString).mkString(","))
          }
           
        case e: InstantiateTypeVars =>
          new Descriptor {
            def basicInfo = "Instantiate type variables to their constraints"
            def fullInfo  = {
              "Instantiate type variables: %tpe".dFormat(Some("Type variables instantiation"),
                e.tvars.map(tvar => {val tvar0 = TypeSnapshot.mapOver(tvar); anyString(tvar0) + " => " +
                                     snapshotAnyString(tvar0.constr.inst)}).mkString("[", ",", "]"))
            }
          }
           
        case e: InstantiateTypeVar =>
          val tvar1 = TypeSnapshot.mapOver(e.tvar)
          new Descriptor {
            def basicInfo = "Instantiate type variable " + anyString(tvar1)
            def fullInfo  = "Instantiate type variable %tpe with constraint %tpe".dFormat(Some("Type variable instantiation"),
                anyString(tvar1), snapshotAnyString(tvar1.constr))
          }
           
        case e: SolveSingleTVar =>
          new Descriptor {
            def basicInfo = "Solving type variable " + snapshotAnyString(e.tvar)
            def fullInfo  = "Solving type variable %tpe %sym".dFormat(snapshotAnyString(e.tvar),
                            (if (!e.variance) "\nType variable is in contravariant position" else ""))
          }
           
        case e: SetInstantiateTypeConstraint =>
          new Descriptor {
            def basicInfo = "Instantiate type constraint as " + snapshotAnyString(e.tp) + " for type variable " + snapshotAnyString(e.tvar) //+ "\n=> " + tvarSetInstExpl(e.reason)
            def fullInfo  = "Instantiated type constraint for type variable %tpe".
              dFormat(Some("Instantiate type constraint"), snapshotAnyString(e.tvar))
          }
           
        case e: WildcardLenientTArg =>
          new Descriptor {
            def basicInfo = if (e.noInstance)
                "Cannot infer an instance for type variable " + snapshotAnyString(e.tvar)
              else 
                "No constraints exist for type variable " + snapshotAnyString(e.tvar)
            def fullInfo  = ""
          }
            
        case e: IncompatibleResultAndPrototype =>
          new Descriptor {
            def basicInfo = "Incompatible function result and the expected type"
            def fullInfo  = "Incompatible types:\n%tpe vs. %tpe".dFormat(Some("Incompatible types"),
                snapshotAnyString(e.restpe), snapshotAnyString(e.pt))
          }
          
        case e: InstantiateGlbOrLub =>
          val instType = if (e.up) "greatest lower bound (glb)" else "least upper bound (lub)"
          def pos = if (e.up) "non-contravariant position" else "contravariant position"
          new Descriptor {
            def tvar1 = TypeSnapshot.mapOver(e.tvar)
            lazy val bounds = if (e.up) tvar1.constr.hiBounds else tvar1.constr.loBounds
            def basicInfo = {
              val constraintsStr = if (bounds.isEmpty) "empty set of constraints" else ("the set of " + bounds.length + " constraints")
              "Type variable is in " + pos + ".\n What is the " + instType + "\n of " + constraintsStr + " of " + anyString(tvar1) + "?"
            }
            def fullInfo  = {
              def boundsString(bs: List[Type]) = 
               if (bs.isEmpty) "empty list of constraints" else bs.map(snapshotAnyString).mkString("[", ",", "]")
              ("Calculate " + instType + " for %tpe").dFormat(Some("Calculate " + instType), boundsString(bounds))
            }
          }
           
        case e: InstantiateGlbOrLubDone =>
          new Descriptor {
            def basicInfo = {
              val instType = if (e.up) "Glb" else "Lub"
              instType + " has been calculated as as \n" + safeTypePrint(e.tp, truncate=false)
            }
            def fullInfo  = ""
          }
           
        case e: AddBoundTypeVar =>
          new Descriptor {
            val boundType = if (e.upperBound) "upper" else "lower"
            def basicInfo = {
              "Register " + boundType + " bound constraint " + snapshotAnyString(e.bound) + "\n for type variable " + snapshotAnyString(e.tvar)
            }
            def fullInfo  = {
              ("Register " + boundType + " bound of type %tpe" +
            		" for type variable %tpe").dFormat(Some("Register bound"),
            		snapshotAnyString(e.bound), snapshotAnyString(e.tvar))
            }
          }

        case e:InferExprInstance =>
          new Descriptor {
            def basicInfo = "Can we infer expression instance?"
            def fullInfo  = {
              val snapshotTree = treeAt(e.tree)
              ("Infer expression instance for tree %tree" +
               "and current type %tpe\n" +
               "with undetermined typeparams %tpe and expected type %tpe").dFormat(Some("Infer expression instance"),
               snapshotAnyString(snapshotTree),
               snapshotAnyString(snapshotTree.tpe),
               e.tparams.map(snapshotAnyString).mkString("[", ",", "]"),
               snapshotAnyString(e.pt))
            }
          }
    
        case e:InferMethodInstance =>
          new Descriptor {
            def basicInfo = "Can we infer correct type parameters for method instance\n using previously typed arguments?"
            def fullInfo  = {
              val snapshotTree = treeAt(e.tree)
              ("Infer method instance for expression %tree" +
                "of type %tpe\n" +
                "Given arguments with types that are %tpe and the expected type %tpe\n" +
                "Try to find substitution for type-parameter(s) %tpe " + 
                "so that the result type of the application is compatible with the expected type").dFormat(Some("Infer type parameters"),
                anyString(snapshotTree), snapshotAnyString(snapshotTree.tpe),
                e.args.map(a => {val a0 = treeAt(a); snapshotAnyString(a0.tpe)}).mkString(","), snapshotAnyString(e.pt),
                e.undetparams.map(snapshotAnyString).mkString(","))
            }
          }

        case e:InferredMethodInstance =>
          new Descriptor {
            def basicInfo = "Inferrred method instance"
            def fullInfo  = {
              val t = treeAt(e.tree)
              ("Inferred method instance %tree as %tpe in application of arguments %tpe").dFormat(Some("Inferred method instance"),
               anyString(t), snapshotAnyString(t.tpe),
               e.args.map(arg => {val arg0 = treeAt(arg); anyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")"))
            }
          }

        case e:NoInstanceForMethodTypeParameters   =>
          new Descriptor {
            def basicInfo = "Cannot infer method instance \n as there is no instance \n for type parameter(s)"
            def fullInfo  = ("No instance for type parameters for %tree" +
           "exist so that it can be applied to %tpe\n" +
           "because " + e.exMsg).dFormat(Some("No instance of method type parameters"),
           snapshotAnyString(e.tree), e.args.map(arg => {val arg0 = treeAt(arg); snapshotAnyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")"))
          }
          
        case e: IsArgCompatibleWithFormalMethInfer =>
          new Descriptor {
            def basicInfo = "Is argument compatible with\n its formal parameter?"
            def fullInfo  = {
              "Is the type of the argument %tpe weakly conformant to its formal parameter %tpe?".dFormat(
                Some("Compatibility check"), snapshotAnyString(e.lhs), snapshotAnyString(e.rhs))
            }
          }
        // todo: 
        case e:MethTypeArgsSolve =>
          new Descriptor {
            def basicInfo = "MethTypeArgsDebug"
            def fullInfo  = "Meth type args is: " + e.tparams.map(snapshotAnyString).mkString(",")
          }
           
        // todo:
        case e:SolvedTypes       =>
          new Descriptor {
            def basicInfo = "Solved Types"
            def fullInfo  = "Solved types: " + e.solvedtypes.map(snapshotAnyString).mkString(",")
          }
           
        // todo:
        case e:InferMethodInstanceTypeError       =>
          new Descriptor {
            def basicInfo = "Cannot infer method instance because \n a type error was encountered"
            def fullInfo  = "Type error \n" + e.exMsg + "\n" +
           "when inferring methods instance for \n" + snapshotAnyString(e.tree) + "\n" +
           " and arguments: \n" +
           e.args.map(arg => {val arg0 = treeAt(arg); snapshotAnyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")")
          }

        // todo:
        case e:FailedTypeCompatibilityInInferExprTypeArgs =>
          new Descriptor {
            def basicInfo = "Incompatible types failure"
            def fullInfo  = "Type " + snapshotAnyString(e.tpe) + "\n" +
              "Expected " + snapshotAnyString(e.pt)
          }

        // todo:
        case e:SubstituteTParamsInfer =>
          new Descriptor {
            def basicInfo = "Substitute type parameters"
            def fullInfo  = "Substitute " + e.tparams.map(snapshotAnyString) + " with " + e.targs.map(snapshotAnyString) +
             (if (e.adjust) " with " else " without ") + "type arguments adjustment"
          }
             
        // todo: 
        case e:PolyTypeInstantiationError =>
          new Descriptor {
            def basicInfo = "Cannot instantiate polymorhpic type \n to the expected type"
            def fullInfo  = "Polymorphic expression \n " + snapshotAnyString(e.tree) + "\n" +
              "cannot be instantiated to the expected type.\n" +
              "Found:    " + snapshotAnyString(e.polytype) + "\n" +
              "Expected: " + snapshotAnyString(e.pt) + "\n" +
              "Undetermined type-parameters: " + e.tparams.map(snapshotAnyString).mkString(",")
          }
           
        case e:TreeTypeSubstitution =>
          new Descriptor {
            def basicInfo = "Define a type substitution map\n for the inferred instance"
            def fullInfo  = {
              val tpSubst = (e.undet zip e.targs).map(subst => snapshotAnyString(subst._1) + " => " + snapshotAnyString(subst._2)).mkString("\n")
              ("Perform substitution to infer concrete instance: %tpe\n" +
              "in %tree").dFormat(Some("Type substitution"), tpSubst, snapshotAnyString(e.tree))
            }
          }
          
        case e: SimpleTreeTypeSubstitution =>
          new Descriptor {
            def basicInfo = "Substitute newly inferred type parameters\nin the expression tree"
            def fullInfo  = {
              "Substitute %tpe for %tpe".dFormat(Some("Type parameters substitution"),
              e.tparams.map(snapshotAnyString).mkString(","),
              e.targs.map(snapshotAnyString).mkString(","))
            }
              
          }
           
        case e:IsApplicableInfer =>
          new Descriptor {
            def basicInfo = "Is function applicable to\n arguments and expected type"
            def fullInfo  = "Is function applicable to argument types and expected type %tpe".dFormat(Some("Is function applicable"), 
                snapshotAnyString(e.pt))
          }
           
        case e:IsApplicableFallbackInfer =>
          new Descriptor {
            def basicInfo = "Fallback: Is function applicable to\n arguments and no expected type"
            def fullInfo  = "Function applicability check failed with expected type %tpe so try with no expected type".dFormat(
                Some("Applicability fallback"), snapshotAnyString(e.pt))
          }
           
        case e:AdjustedTypeArgs =>
          DEFAULT
          
        case e:InferMethodAlternative =>
          new Descriptor {
            def basicInfo = "Can we infer the specific method\n" + (if (e.implicits) "when implicits are enabled" else "without involving implicits") + "?"
            def fullInfo  = ("Can we infer a specific method for %tree of type %tpe?" +
                             "Available alternatives have types: %tpe\n" +
                             "The context has undetermined type parameters %tpe\n" +
                             "and an expected type %tpe").dFormat(Some("Infer correct method alternative"),
                              snapshotAnyString(e.tree), combinedSnapshotAnyString(e.tree)(_.tpe),
                              e.argsTpes.map(snapshotAnyString).mkString(","), e.tparams.map(snapshotAnyString).mkString(","),
                              snapshotAnyString(e.pt))
          }
           
        case e:NoBestAlternativeTryWithWildcard =>
          new Descriptor {
            def basicInfo = "No single best implict was found.\n Try searching without expected type"
            def fullInfo  = ("Couldn't find best alternative for %tree %sym," +
            		"will attempt to infer best alternative without expected type.%sym").dFormat(
            		snapshotAnyString(e.tree),
                (if (!e.competing.isEmpty) "Conflict in " + e.competing else ""), 
                (if (!e.applicable.isEmpty) "\nSome applicable implicits are: " + e.applicable else ""))
          }
             
        // todo:
        case e:AmbiguousAlternatives =>
          new Descriptor {
            def basicInfo = "Ambiguous alternatives for method"
            def fullInfo  = snapshotAnyString(e.tree) + "%tree has ambigous alternatives. All applicable alternatives are:\n" +
              e.applicable + " of which the conflicting ones are\n" + e.competing
          }
           
        case e:VerifyMethodAlternative =>
          new Descriptor {
            def basicInfo = "Is method alternative type correct\nfor the given context?"
            def fullInfo  = 
                ("Check if method alternative %sym of type %tpe" +
                "is applicable for types of the arguments: %tpe\n" +
                "and conforms to the expected type %tpe").dFormat(Some("Verify alternative"),
                    snapshotAnyString(e.alternative), combinedSnapshotAnyString(e.alternative)(_.tpe),
                    e.argsTypes.map(snapshotAnyString).mkString, snapshotAnyString(e.pt))
          }
           
        case e:PossiblyValidAlternative =>
          val result = if (e.result) "valid" else "invalid"
          new Descriptor {
            def basicInfo = "Alternative is " + result
            def fullInfo  = ("Alternative %sym of type %tpe is " + result).dFormat(Some("Applicability of the alternative"),
              snapshotAnyString(e.alternative), combinedSnapshotAnyString(e.alternative)(_.tpe))
          }
         
        case e:MethodTypeApplicableDebug =>
          DEFAULT
          
        case e:FastTrackMethodTypeApplicableDebug =>
          DEFAULT
    
        case e:PolyTypeApplicableDebug =>
          DEFAULT
    
        case e:TypesCompatibleDebug =>
          DEFAULT
          
        case e:InferExprAlternative =>
          new Descriptor {
            def basicInfo = "Infer expression \n with alternatives \n[" + (if (e.implicits) "with implicits" else "without implicits")+ "]"
            def fullInfo  = {
              val snapshotTree = treeAt(e.tree)
              ("Expression %tree has multiple alternatives and overloaded type %tpe." +
              "Try to infer correct alternative that matches expected type %tpe").dFormat(
                anyString(snapshotTree), snapshotAnyString(snapshotTree.tpe), snapshotAnyString(e.pt))
            }
          }
           
        case e:ImprovesAlternativesCheck =>
          new Descriptor {
            def basicInfo = "Compare conflicting alternatives"
            def fullInfo  = "Compare conflicting alternatives, both applicable for %tree".dFormat(
                Some("Compare alternatives"), snapshotAnyString(e.tree))
          }
           
        case e:AlternativesCheck =>
          new Descriptor {
            def basicInfo = if (e.check) "Alternative improved" else "Alternatives are comparable"
            def fullInfo  = ""
          }

        case _ =>
          DEFAULT

      }
    }
    
    def explainLubGlbEvent(ev: Event with LubEvent)(implicit time: Clock = ev.time): Descriptor = ev match { 
      case e: CalcLub =>
        new Descriptor {
          def basicInfo = "Calculate least upper bound " + lubKindExpl1(e.kind)
          def fullInfo  = "Calculating least upper bound for types: %tpe".dFormat(Some("Calculating LUB"), e.tps.map(snapshotAnyString(_)).mkString(","))
        }

      case e: CalcLubElimSubTypes =>
        new Descriptor {
          def basicInfo = "What is the least upper bound of type constraints\n (after performing subtype elimination on the original list)?" + lubKindExpl2(e.kind)
          def fullInfo  = "Calculating least upper bound for types: %tpe".dFormat(Some("Calculating LUB"), e.tps.map(snapshotAnyString(_)).mkString(","))
        }

      case _ =>
        DEFAULT

    }
    
    def lubKindExpl1(v: LubKindEntry.Value): String = {
      import LubKindEntry._
      v match {
        case Empty      =>
          "\nSolution: Maximize lub for empty list of constraints"
        case SingleElem =>
          "\nSolution: lub of a single type constraint"
        case NonTrivial =>
          ""
      }      
    }
    
    
    def lubKindExpl2(v: LubKindElimSubtypes.Value): String = {
      import LubKindElimSubtypes._
      v match {
        case Empty            =>
          "\nsolution: Maximize lub for empty list of constraints"
        case SingleElem       =>
          "\nsolution: lub of a single type constraint"
        case PolyTpe          =>
          "for Polymorphic type"
        case MethodTpe        =>
          "for Method type"
        case NullaryMethodTpe =>
          "for Nullary method type"
        case TpeBounds =>
          "for Type bounds"
        case NonTrivial =>
          "\nsolution: Refined type for lub"
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