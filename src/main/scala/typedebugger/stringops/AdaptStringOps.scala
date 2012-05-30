package scala.typedebugger
package stringops

trait AdaptStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  import util.StringFormatter._
  
  trait AdaptEventsOps {
    self: Descriptors =>
    private val DEFAULT = new DefaultDescriptor("adapt")
      
    def explainAdaptEvent(ev: Event with AdaptEvent)(implicit time: Clock = ev.time): Descriptor = ev match {
      case e:AdaptStart =>
        new Descriptor {
          def basicInfo = if (e.pt == WildcardType) "Adapt expression \nwith no expected type" 
                          else "Can we adapt expression to the expected type" + safeTypePrint(e.pt, ":\n", "", truncate=false) + "?"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            //val resTpe = TypeSnapshot.mapOver(e.resTree._1.tpe)(e.resTree._2)
            val resTree = treeAt(e.resTree._1)(e.resTree._2)
            val resTpe = TypeSnapshot.mapOver(resTree.tpe)(e.resTree._2)
            
            ("Can we adapt the type of the expression (if necessary) to the expected type.\n" + 
             "Found: %tpe" +
             "Expected: %tpe" +
             "\n\nType of the tree after adaptation: %tpe").dFormat(Some("Adapt expression"),
              snapshotAnyString(tree1.tpe), snapshotAnyString(e.pt), anyString(resTpe))
          }
        }
        
      case e:AdaptDone =>
        DEFAULT
        
      case e:MainAdaptDone =>
        new Descriptor {
          def basicInfo = "Debug: Main adapt done"
          def fullInfo = {
            val tree1 = treeAt(e.resTree)
            ("Resulting tree %tree is of type %tpe").dFormat(anyString(tree1), snapshotAnyString(tree1.tpe))
          }
        }

      case e:AdaptAnnotationsAdapt =>
        DEFAULT
        
      case e:ConstantTpeAdapt =>
        new Descriptor {
          def basicInfo = "Can we adapt constant value?"
          def fullInfo  = ""
        }
        
      case e:OverloadedTpeAdapt =>
        DEFAULT
        
      case e:NullaryMethodTypeAdapt =>
        new Descriptor {
          def basicInfo = "Can we adapt nullary method type"
          def fullInfo  = ("Adapt nullary method type with underlying type " +
          		            "type %tpe and undetermined context parameters %sym").dFormat(
          		                Some("Nullary method type adaptation"),
          		                snapshotAnyString(e.tpe),
          		                e.undetTParams.map(snapshotAnyString).mkString("[", ",", "]"))
        }
        
      case e:ByNameParamClassAdapt =>
        DEFAULT
        
      case e:SkolemizeTpe1Adapt =>
        DEFAULT
        
      case e:SkolemizeTpe2Adapt =>
        DEFAULT
       
      case e:PolyTpeAdapt =>
        new Descriptor {
          def basicInfo = "Can we adapt an expression with a polymorphic type?"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            ("Adapt polymorphic type %tpe\n" +
            "Type-parameters: %tpe" + 
            "Result type: %tpe" + 
            "For type tree: %tree" + 
            "Undetermined context type-parameters: %sym").dFormat(Some("Adapt an expression with polymorphic type"),
                snapshotAnyString(tree1.tpe), e.tparams.map(snapshotAnyString).mkString(","), snapshotAnyString(e.restpe),
                snapshotAnyString(e.typeTree), e.undetTParams.map(snapshotAnyString).mkString(","))
          }
        }

      case e:ImplicitMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Expression tree has a MethodType with implicit parameter(s).\n" +
          		            "Can we find and apply implicit arguments?"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            ("Adapt expression %tree" +
            "having MethodType and implicit parameters %tpe. " +
            "We need to find implicit argument(s) in the context and apply it").dFormat(Some("Adapt method type with implicit parameters"),
            anyString(tree1),  snapshotAnyString(tree1.tpe))
          }
        }
         
      // todo: not used?
      case e:UndetParamsMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Infer expression instance\n for implicit method type"
          def fullInfo  = ""
        }
        
      // todo: not used?
      case e:SuccessSilentMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Successfully typed application\n of (inferred) implicit arguments"
          def fullInfo  = "Typed successfully application of inferred arguments for \n" +
            snapshotAnyString(e.tree) + "\n" + "with type " + snapshotAnyString(e.tpe)
        }
         
      case e:InferImplicitForParamAdapt =>
        val param1 = SymbolSnapshot.mapOver(e.param)
        new Descriptor {
          def basicInfo = "Is there an implicit argument for\n the '" + anyString(param1) + "' parameter"// + safeTypePrint(param1.tpe, "\nof type: ", "")
          def fullInfo  = "Infer implicit argument for parameter %sym of type %tpe".dFormat(Some("Infer implicit argument"),
                           anyString(param1), snapshotAnyString(param1.tpe))
        }
         
      case e:InferDivergentImplicitValueNotFound =>
        new Descriptor {
          def basicInfo = "Implicit search failed due to diverging implicits"
          def fullInfo  = {
            val param1 = SymbolSnapshot.mapOver(e.param)
            "Could not find implicit value for evidence parameter %sym of type %tpe".dFormat(Some("Failed implicit search"),
              param1.name.toString, snapshotAnyString(param1.tpe))
          }
        }
         
      case e:InferImplicitValueNotFound =>
        new Descriptor {
          def basicInfo = "No implicit was found"
          def fullInfo  = {
            val param1 = SymbolSnapshot.mapOver(e.param)
            "Could not find implicit value for evidence parameter %sym of type %tpe".dFormat(Some("No implicit was found"),
              param1.name.toString, snapshotAnyString(param1.tpe))
          }
        }
         
      case e:InferredImplicitAdapt =>
        new Descriptor {
          def basicInfo = "Implicit argument has been applied in the application"
          def fullInfo  = ""
        }
       
      // todo: not used?
      case e:EtaMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Eta expansion"
          def fullInfo  = "Eta expansion of tree " + snapshotAnyString(e.tree) +
                          " with expected type: " + snapshotAnyString(e.pt)
        }
         
      case e:TreeAfterEtaExpansionMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Original tree has been transformed into\n its eta-expanded form"
          def fullInfo  = "%tree has been eta-expanded to %tree".dFormat(Some("Result of eta-expansion"),
              snapshotAnyString(e.tree), snapshotAnyString(e.tree1))
        }

      case e:InstantiateTParamsForEtaExpansionAdapt =>
        new Descriptor {
          def basicInfo = "Can we instantiate type parameters \n in eta expansion"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            "Can we instantiate type parameters %tpe in %tree of type %tpe?".dFormat(Some("Instantiate eta-expanded tree"),
            e.tparams.map(snapshotAnyString).mkString(","), anyString(tree1), snapshotAnyString(tree1.tpe)) 
          }
        }

      // todo: not used?
      case e:InferExprFailed =>
        new Descriptor {
          def basicInfo = "Failed to infer expression instance"
          def fullInfo  = snapshotAnyString(e.tree) + " with expected type " +
            snapshotAnyString(e.pt) + "\nFailed with type error " + e.e
        }
         
      // todo: improve
      case e:ApplyNullaryMethodAdapt =>
        new Descriptor {
          def basicInfo = "Can we adapt nullary MethodType\n by performing 'apply' adaptation?"
          def fullInfo  = snapshotAnyString(e.tree) + ": " + snapshotAnyString(e.methTpe)
        }
      
      // todo: not used?
      case e:TypeTreeAdapt =>
        new Descriptor {
          def basicInfo = "Can we adapt type tree?"
          def fullInfo  = ""
        }

      case e:FunModeAdapt =>
        DEFAULT
        
      case e:ConvertToTypeTreeAdapt =>
        DEFAULT // internal
       
      case e:PatternConstructorsAdapt =>
        new Descriptor {
          def basicInfo = "Adapt pattern constructor"
          def fullInfo  = ""
        }
        
      case e:ApplyAdapt =>
        new Descriptor {
          def basicInfo = "Expression is involved in application\n and needs to be adapted to contain an 'apply' member"
          def fullInfo  = ""
        }
        
      case e:AdaptToNameQualAdapt =>
        DEFAULT
        
      // todo:
      case e:InferInstanceAdapt =>
        new Descriptor {
          def basicInfo = e.e match {
           case DefaultExplanation =>
             "Adapt by inferring concrete instance"
           case _ =>
             "Can we infer a concrete instance\n by resolving undetermined type parameters?" //and \n " + explainNamer(e.e)
          }
          def fullInfo  = {
            "Can we instantiate undetermined paratemers %tpe that exist in expression %tree in the context of expected type %tpe".dFormat(
              Some("Infer concrete instance"), e.tparams.map(snapshotAnyString).mkString("[", ",", "]"), snapshotAnyString(e.tree),
              snapshotAnyString(e.pt))
          }
        }

      case e:SuccessSubTypeAdapt =>
        new Descriptor {
          def basicInfo = "Subtyping constraint satisfied"
          def fullInfo  = "Constraint satisfied %tpe <:< %tpe\n for tree %tree".dFormat(
                          Some("Subtyping constraint satisfied"),
                          snapshotAnyString(e.value1),
                          snapshotAnyString(e.value2),
                          snapshotAnyString(e.tree))
        }
         
      case e:ConstantFoldSubTypeAdapt =>
        new Descriptor {
          def basicInfo = "Subtype constraint for constants\n satisfied"
          def fullInfo  = ""
        }
        
      case e:NotASubtypeAdapt => 
        new Descriptor {
          def basicInfo = "Can we adapt the type of the expression\n to satisfy subtyping constraint\n"+
            safeTypePrint(e.tpe, truncate=false) + " <: " + safeTypePrint(e.pt, truncate=false) + "?"
          def fullInfo  = "FAILED subtype constraint: %tpe <:< %tpe".dFormat(Some("Failed subtyping contstraint"),
            snapshotAnyString(e.tpe), snapshotAnyString(e.pt))
        }

      case e:AdaptToUnitAdapt =>
        new Descriptor {
          def basicInfo = "Can we adapt to Unit?"
          def fullInfo  = "Can we adapt type %tpe to Unit?".dFormat(Some("Unit adaptation"), snapshotAnyString(e.tpe))
        }
        
      case e:WeakConformanceAdapt =>
        new Descriptor {
          def basicInfo = "Can we adapt numeric value\n that only weakly conforms to the type?"
          def fullInfo  = ""
        }
        
      case e:AnnotationCheckerAdapt =>
        DEFAULT
        
      // todo:
      case e:InstantiateAdapt =>
        new Descriptor {
          def basicInfo = "Can we instantiate undetermined type parameters?"
          def fullInfo  = ""
        }
        
      case e:FindViewAdapt =>
        new Descriptor {
          def basicInfo = "Search for a view that satisfies\n subtype constraint"
          def fullInfo  =
            ("Find view that adapts expression %tree so that its type conforms " +
            "to the expected type").dFormat(Some("Search view for subtype constraint"), snapshotAnyString(e.tree))
        }
        
      case e:ApplyViewAdapt =>
        new Descriptor {
          def basicInfo = "Can we typecheck the application of the found view?"
          def fullInfo  = ("Can we verify that the the view %tree can be really" +
          		"applied to the original argument?").dFormat(Some("Verify application of the view"),
          		 snapshotAnyString(e.coercion))
        }
         
      // todo:
      case e:NoViewFound =>
        new Descriptor {
          def basicInfo = "No applicable view was found"
          def fullInfo  = "No view was found for tree\n" + snapshotAnyString(e.tree) + "\n" +
            "of type " + snapshotAnyString(e.tree.tpe) +
            " that conforms to the expected type " + snapshotAnyString(e.pt)
        }

      case _ =>
        DEFAULT

    }
  }
}