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
                          else "Adapt to the expected type" + safeTypePrint(e.pt, ":\n", "", truncate=false)
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            ("Adapt the type of the expression (if necessary) to the expected type.\n" + 
             "Found: %tpe" +
             "Expected: %tpe" +
             "\n\nResulting type of the tree: %tpe").dFormat(Some("Adapt expression"),
              snapshotAnyString(tree1.tpe), snapshotAnyString(e.pt), anyString(e.tree.tpe))
          }
        }
        
      case e:AdaptDone =>
        DEFAULT

      case e:AdaptAnnotationsAdapt =>
        DEFAULT
        
      case e:ConstantTpeAdapt =>
        new Descriptor {
          def basicInfo = "Adapt constant value"
          def fullInfo  = ""
        }
        
      case e:OverloadedTpeAdapt =>
        DEFAULT
        
      case e:NullaryMethodTypeAdapt =>
        new Descriptor {
          def basicInfo = "Adapt nullary method type"
          def fullInfo  = ""
        }
        
      case e:ByNameParamClassAdapt =>
        DEFAULT
        
      case e:SkolemizeTpe1Adapt =>
        DEFAULT
        
      case e:SkolemizeTpe2Adapt =>
        DEFAULT
       
      case e:PolyTpeAdapt =>
        new Descriptor {
          def basicInfo = "Adapt an expression with polymorphic type"
          def fullInfo  =
            ("Type-parameters: %tpe" + 
            "Result type: %tpe" + 
            "For type tree: %tree" + 
            "Undetermined context type-parameters: %sym").dFormat(Some("Adapt an expression with polymorphic type"),
                e.tparams.map(snapshotAnyString).mkString(","), snapshotAnyString(e.tpe),
                snapshotAnyString(e.typeTree), e.undetTParams.map(snapshotAnyString).mkString(","))
        }

      case e:ImplicitMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Adapt expression with method type\n and implicit parameter(s)"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            "Adapt expression \n" + anyString(tree1) + "\n" +
            "having method type and implicit parameters " + snapshotAnyString(tree1.tpe) + "\n" +
            "Needs to find implicit argument in the context and apply it"
          }
        }
         
      case e:UndetParamsMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Infer expression instance\n for implicit method type"
          def fullInfo  = ""
        }
        
      case e:SuccessSilentMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Successfully typed application\n of (inferred) implicit arguments"
          def fullInfo  = "Typed successfully application of inferred arguments for \n" +
            snapshotAnyString(e.tree) + "\n" + "with type " + snapshotAnyString(e.tpe)
        }
         
      case e:InferImplicitForParamAdapt =>
        val param1 = SymbolSnapshot.mapOver(e.param)
        new Descriptor {
          def basicInfo = "Infer implicit for parameter " + safeTypePrint(param1.tpe, "\nof type: ", "")
          def fullInfo  = "Infer implicit for parameter '" + anyString(param1) + "': " +
                            snapshotAnyString(param1.tpe)
        }
         
      case e:InferDivergentImplicitValueNotFound =>
        new Descriptor {
          def basicInfo = "Implicit value not found"
          def fullInfo  = {
            val param1 = SymbolSnapshot.mapOver(e.param)
            "Could not find implicit value for evidence parameter \n" + param1.name + 
            "\nof type " + snapshotAnyString(param1.tpe)
          }
        }
         
      case e:InferImplicitValueNotFound =>
        new Descriptor {
          def basicInfo = "Implicit value not found"
          def fullInfo  = {
            val param1 = SymbolSnapshot.mapOver(e.param)
            "Could not find implicit value for evidence parameter \n" + param1.name + 
            "\nof type " + snapshotAnyString(param1.tpe)
          }
        }
         
      case e:InferredImplicitAdapt =>
        new Descriptor {
          def basicInfo = "Finished applying inferred \n implicit argument (if any)"
          def fullInfo  = ""
        }
        
      case e:EtaMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Eta expansion"
          def fullInfo  = "Eta expansion of tree " + snapshotAnyString(e.tree) +
                          " with expected type: " + snapshotAnyString(e.pt)
        }
         
      case e:TreeAfterEtaExpansionMethodTpeAdapt =>
        new Descriptor {
          def basicInfo = "Eta-expanded tree"
          def fullInfo  = snapshotAnyString(e.tree) + "\nhas been eta-expanded to\n " +
                          snapshotAnyString(e.tree1)
        }

      case e:InstantiateTParamsForEtaExpansionAdapt =>
        new Descriptor {
          def basicInfo = "Instantiate type-parameters \n in eta expansion"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            anyString(tree1) + " with type " + snapshotAnyString(tree1.tpe) +
            "\nTree's symbol " + snapshotAnyString(e.meth) + " with type parameters '" +
            e.tparams.map(snapshotAnyString) + "' to instantiate"
          }
        }

      case e:InferExprFailed =>
        new Descriptor {
          def basicInfo = "Failed to infer expression instance"
          def fullInfo  = snapshotAnyString(e.tree) + " with expected type " +
            snapshotAnyString(e.pt) + "\nFailed with type error " + e.e
        }
         
      case e:ApplyNullaryMethodAdapt =>
        new Descriptor {
          def basicInfo = "Apply nullary method"
          def fullInfo  = snapshotAnyString(e.tree) + ": " + snapshotAnyString(e.methTpe)
        }
         
      case e:TypeTreeAdapt =>
        new Descriptor {
          def basicInfo = "Adapt type tree"
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
          def basicInfo = "Adapt expression to contain \n apply() member"
          def fullInfo  = ""
        }
        
      case e:AdaptToNameQualAdapt =>
        DEFAULT
        
      case e:InferInstanceAdapt =>
        new Descriptor {
          def basicInfo = e.e match {
           case DefaultExplanation =>
             "Adapt by inferring concrete instance"
           case _ =>
             "Infer concrete instance" //and \n " + explainNamer(e.e)
          }
          def fullInfo  = {
            "Instantiate undetermined paratemers " + e.tparams.map(snapshotAnyString).mkString("[", ",", "]") + "\n" +
            (e.e match {
              case DefaultExplanation =>
                " and adapt by inferring concrete instance"
              case _ =>
                e.e // TODO
             }) + "\n" +
            "in expression \n" + snapshotAnyString(e.tree) + "\n" + 
           "with expected type: " + snapshotAnyString(e.pt)
          }
        }

      case e:SuccessSubTypeAdapt =>
        new Descriptor {
          def basicInfo = "Subtype constraint satisfied"
          def fullInfo  = "Constraint satisfied \n" + snapshotAnyString(e.value1) +
            " <:< " + snapshotAnyString(e.value2) + "\n in tree " + snapshotAnyString(e.tree)
        }
         
      case e:ConstantFoldSubTypeAdapt =>
        new Descriptor {
          def basicInfo = "Subtype constraint for constants\n satisfied"
          def fullInfo  = ""
        }
        
      case e:NotASubtypeAdapt => 
        new Descriptor {
          def basicInfo = "Adapt expression's type to satisfy\n" +
            safeTypePrint(e.tpe, truncate=false) + " <: " + safeTypePrint(e.pt, truncate=false)
          def fullInfo  = "FAILED subtype constraint:\n " +
            snapshotAnyString(e.tpe) + " <: " + snapshotAnyString(e.pt)
        }

      case e:AdaptToUnitAdapt =>
        new Descriptor {
          def basicInfo = "Adapt to Unit"
          def fullInfo  = "Adapt to Unit type \n " + snapshotAnyString(e.tpe)
        }
        
      case e:WeakConformanceAdapt =>
        new Descriptor {
          def basicInfo = "Weak conformance adapt\n (numeric values)"
          def fullInfo  = ""
        }
        
      case e:AnnotationCheckerAdapt =>
        DEFAULT
        
      case e:InstantiateAdapt =>
        new Descriptor {
          def basicInfo = "Instantiate undetermined parameters"
          def fullInfo  = ""
        }
        
      case e:FindViewAdapt =>
        new Descriptor {
          def basicInfo = "Search for view that satisfies\n subtype constraint"
          def fullInfo  = "Find view that adapts initial expression's type to the type\n" +
            "that conforms to the expected type"
        }
        
      case e:ApplyViewAdapt =>
        new Descriptor {
          def basicInfo = "Apply (found) applicable view"
          def fullInfo  = "Found view \n " + snapshotAnyString(e.coercion) + "\n" +
            "which is applicable for the original argument"
        }
         
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