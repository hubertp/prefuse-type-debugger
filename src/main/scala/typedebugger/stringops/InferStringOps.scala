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
            def fullInfo  = snapshotAnyString(e.tree)
          }
           
        case e:MethodInfer =>
          DEFAULT

        case e:AdjustTypeArgs =>
          DEFAULT
          
        case e:CompatibleTypes =>
          new Descriptor {
            def basicInfo = "Compatible types,\ntry to solve any type variables"
            def fullInfo  =
              "Compatible types \n" +
              "Found:    " + snapshotAnyString(e.found) + "\n" +
              "Required: " + snapshotAnyString(e.pt) + "\n" +
              "with type parameters " + e.tparams.map(snapshotAnyString).mkString(",")
          }
           
        case e: InstantiateTypeVars =>
          new Descriptor {
            def basicInfo = "Instantiate type variables"
            def fullInfo  = "Instantiate type variables: " +
                            e.tvars.map(tvar => {val tvar0 = TypeSnapshot.mapOver(tvar); anyString(tvar0) + " => " +
                                                 snapshotAnyString(tvar0.constr.inst)}).mkString("[", ",", "]")
          }
           
        case e: InstantiateTypeVar =>
          val tvar1 = TypeSnapshot.mapOver(e.tvar)
          new Descriptor {
            def basicInfo = "Instantiate type variable " + anyString(tvar1)
            def fullInfo  = "Instantiate type variable: " + anyString(tvar1) +
                            " with constraint " + snapshotAnyString(tvar1.constr)
          }
           
        case e: SolveSingleTVar =>
          new Descriptor {
            def basicInfo = "Solve type variable"
            def fullInfo  = "Solve: " + snapshotAnyString(e.tvar) + "\n" +
                            (if (!e.variance) "contravariant position" else "")
          }
           
        case e: SetInstantiateTypeConstraint =>
          new Descriptor {
            def basicInfo = "Set instantiation for type constraint\n=> " + tvarSetInstExpl(e.reason)
            def fullInfo  = "Instantiated type constraint for type variable %tpe".
              dFormat(Some("Instantiate type constraint"), snapshotAnyString(e.tvar))
          }
           
        case e: WildcardLenientTArg =>
          new Descriptor {
            def basicInfo = if (e.noInstance)
                "No instance of a typevariable " + snapshotAnyString(e.tvar)
              else 
                "No type var constraint instance for " + snapshotAnyString(e.tvar)
            def fullInfo  = ""
          }
            
        case e: IncompatibleResultAndPrototype =>
          new Descriptor {
            def basicInfo = "Incompatible function result- and proto-type"
            def fullInfo  = "Incompatible types:\n" + snapshotAnyString(e.restpe) +
                            " vs.\n" + snapshotAnyString(e.pt)
          }
          
        case e: InstantiateGlbOrLub =>
          val instType = if (e.up) "greater lower bound (glb)" else "lower upper bound (lub)"
          def pos = if (e.up) "non-contravariant position" else "contravariant position"
          def tvar1 = TypeSnapshot.mapOver(e.tvar)
          new Descriptor {
            def basicInfo = "Calculate " + instType + "\nbecause type parameter is in\n" + pos
            def fullInfo  = {
              
              lazy val bounds = if (e.up) tvar1.constr.hiBounds else tvar1.constr.loBounds
              def boundsString(bs: List[Type]) = 
               if (bs.isEmpty) "empty bounds" else bs.map(snapshotAnyString).mkString("[", ",", "]")
              "Calculate " + instType + " for " + boundsString(bounds)
            }
          }
           
        case e: InstantiateGlbOrLubDone =>
          new Descriptor {
            def basicInfo = {
              val instType = if (e.up) "glb" else "lub"
              "Calculated " + instType + " as \n" + safeTypePrint(e.tp, truncate=false)
            }
            def fullInfo  = ""
          }
           
        case e: AddBoundTypeVar =>
          new Descriptor {
            val boundType = if (e.upperBound) "upper" else "lower"
            def basicInfo = {
              "Register " + boundType + " bound\nof type " + snapshotAnyString(e.bound)
            }
            def fullInfo  = "Register " + boundType + " bound of type %tpe" +
            		"\nfor type variable %tpe".dFormat(Some("Register bound"),
            		snapshotAnyString(e), snapshotAnyString(e.tvar))
          }

        case e:InferExprInstance =>
          new Descriptor {
            def basicInfo = "Infer expression instance"
            def fullInfo  = {
              val snapshotTree = treeAt(e.tree)
              "Infer expression instance for tree \n" +
                snapshotAnyString(snapshotTree) + "\n" +
//           " of type '" + snapshotAnyString(snapshotTree.tpe) + "' \n" +
                "with undetermined typeparams '" + e.tparams.map(snapshotAnyString).mkString(",") + "' \n" +
                "and expected type " + snapshotAnyString(e.pt)
            }
          }
    
        case e:InferMethodInstance =>
          new Descriptor {
            def basicInfo = "Infer method instance \n using inferred argument types"
            def fullInfo  = {
              val snapshotTree = treeAt(e.tree)
              "Infer method instance for expression \n " +
                anyString(snapshotTree) + "\n" +
                "of type " + snapshotAnyString(snapshotTree.tpe) + "\n" +
                "Given arguments with types are " + e.args.map(a => {val a0 = treeAt(a); snapshotAnyString(a0.tpe)}).mkString(",") + "\n" +
                "and the expected type " + snapshotAnyString(e.pt) + "\n" +
                "Try to find substitution for type-parameter(s) " + e.undetparams.map(snapshotAnyString).mkString(",") + "\n" +
                "so that the result type of the application is compatible with the expected type" + snapshotAnyString(e.pt)
            }
          }

        case e:InferredMethodInstance =>
          new Descriptor {
            def basicInfo = "Inferrred method instance"
            def fullInfo  = "Inferred method instance \n" + 
              snapshotAnyString(e.tree) + "\n" +
              "in application of arguments: \n" +
              e.args.map(arg => {val arg0 = treeAt(arg); anyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")")
          }

        case e:NoInstanceForMethodTypeParameters =>
          new Descriptor {
            def basicInfo = "Cannot infer method instance \n as there is no instance \n for type parameter(s)"
            def fullInfo  = "No instance exists for type parameters for \n" +
           snapshotAnyString(e.tree) + "\n" +
           "exist so that it can be applied to \n" +
           e.args.map(arg => {val arg0 = treeAt(arg); snapshotAnyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")") + "\n" +
           "because " + e.exMsg
          }
           
        case e:MethTypeArgsSolve =>
          new Descriptor {
            def basicInfo = "MethTypeArgsDebug"
            def fullInfo  = "Meth type args is: " + e.tparams.map(snapshotAnyString).mkString(",")
          }
           
        case e:SolvedTypes =>
          new Descriptor {
            def basicInfo = "Solved Types"
            def fullInfo  = "Solved types: " + e.solvedtypes.map(snapshotAnyString).mkString(",")
          }
           
        case e:InferMethodInstanceTypeError =>
          new Descriptor {
            def basicInfo = "Cannot infer method instance \n as encountered type error"
            def fullInfo  = "Type error \n" + e.exMsg + "\n" +
           "when inferring methods instance for \n" + snapshotAnyString(e.tree) + "\n" +
           " and arguments: \n" +
           e.args.map(arg => {val arg0 = treeAt(arg); snapshotAnyString(arg0) + ":" + snapshotAnyString(arg0.tpe)}).mkString("(", ",", ")")
          }

        case e:FailedTypeCompatibilityInInferExprTypeArgs =>
          new Descriptor {
            def basicInfo = "Incompatible types failure"
            def fullInfo  = "Type " + snapshotAnyString(e.tpe) + "\n" +
              "Expected " + snapshotAnyString(e.pt)
          }

        case e:SubstituteTParamsInfer =>
          new Descriptor {
            def basicInfo = "Substitute type parameters"
            def fullInfo  = "Substitute " + e.tparams.map(snapshotAnyString) + " with " + e.targs.map(snapshotAnyString) +
             (if (e.adjust) " with " else " without ") + "type arguments adjustment"
          }
             
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
            def basicInfo = "Type substitution map \n for inferred instance"
            def fullInfo  = {
              val tpSubst = (e.undet zip e.targs).map(subst => snapshotAnyString(subst._1) + " => " + snapshotAnyString(subst._2)).mkString("\n")
              "Perform substitution to infer concrete instance:\n" + tpSubst + "\n" +
              "in " + snapshotAnyString(e.tree)
            }
          }
           
        case e:IsApplicableInfer =>
          new Descriptor {
            def basicInfo = "Is function applicable to\n arguments and expected type"
            def fullInfo  = "Is function applicable to argument types and expected type " + snapshotAnyString(e.pt)
          }
           
        case e:IsApplicableFallbackInfer =>
          new Descriptor {
            def basicInfo = "Fallback: Is function applicable to\n arguments and no expected type"
            def fullInfo  = "Is applicable check failed with expected type " + snapshotAnyString(e.pt) +
              " so try with no expected type"
          }
           
        case e:AdjustedTypeArgs =>
          DEFAULT
          
        case e:InferMethodAlternative =>
          new Descriptor {
            def basicInfo = "Infer method \n with alternatives\n [" + (if (e.implicits) "with implicits" else "without implicits")+ "]"
            def fullInfo  = "In " + snapshotAnyString(e.tree) + " with overloaded type\n " +
              combinedSnapshotAnyString(e.tree)(_.tpe) + "\n" +
              "we infer the correct, single method from alternatives suitable for " + e.argsTpes.map(snapshotAnyString) +
              " with undetermined type parameters " + e.tparams +
              "\nand expected final type " + snapshotAnyString(e.pt)
          }
           
        case e:NoBestAlternativeTryWithWildcard =>
          new Descriptor {
            def basicInfo = "No single best implict was found.\n Try searching without expected type"
            def fullInfo  = "Couldn't find best alternative for " + snapshotAnyString(e.tree) + ".\n" + 
             (if (!e.competing.isEmpty) "Conflict in " + e.competing else "") +
              ", will attempt to infer best alternative without pt.\n" +
             (if (!e.applicable.isEmpty) "Some applicable implicits are: " + e.applicable else "")
          }
             
        case e:AmbiguousAlternatives =>
          new Descriptor {
            def basicInfo = "Ambiguous alternatives for method"
            def fullInfo  = snapshotAnyString(e.tree) + "\n has ambigous alternatives. All applicable alternatives are:\n" +
              e.applicable + " of which the conflicting ones are\n" + e.competing
          }
           
        case e:VerifyMethodAlternative =>
          new Descriptor {
            def basicInfo = "Verify alternative"
            def fullInfo  = 
                "Check if method alternative %sym of type %tpe" +
                "is applicable for types of the arguments: %tpe\n" +
                "and conforms to the expected type %tpe".dFormat(Some("Verify alternative"),
                    snapshotAnyString(e.alternative), combinedSnapshotAnyString(e.alternative)(_.tpe),
                    e.argsTypes.map(snapshotAnyString).mkString, snapshotAnyString(e.pt))
          }
           
        case e:PossiblyValidAlternative =>
          val result = if (e.result) "Valid" else "Invalid"
          new Descriptor {
            def basicInfo = result + " alternative"
            def fullInfo  = "Alternative " + snapshotAnyString(e.alternative) + " with type " +
              combinedSnapshotAnyString(e.alternative)(_.tpe) + " is " + result
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
              "In " + anyString(snapshotTree) + " with overloaded type " + snapshotAnyString(snapshotTree.tpe) +
              " we infer the alternative for type " + snapshotAnyString(e.pt)
            }
          }
           
        case e:ImprovesAlternativesCheck =>
          new Descriptor {
            def basicInfo = "Compare conflicting alternatives"
            def fullInfo  = "Compare conflicting alternatives, both applicable for %tree".dFormat(Some("Compare alternatives"), snapshotAnyString(e.tree))
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
          def basicInfo = "Calculate lub " + lubKindExpl1(e.kind)
          def fullInfo  = "Calculating lub for types: " + e.tps.map(snapshotAnyString(_)).mkString("[", ",", "]")
        }

      case e: CalcLubElimSubTypes =>
        new Descriptor {
          def basicInfo = "Calculate lub after eliminating subtypes " + lubKindExpl2(e.kind)
          def fullInfo  = "Calculating lub for types: " + e.tps.map(snapshotAnyString(_)).mkString("[", ",", "]")
        }

      case _ =>
        DEFAULT

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