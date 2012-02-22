package scala.typedebugger
package processing

trait AdaptStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  
  trait AdaptEventsOps {
    private val DEFAULT = ("(adapt| not-implemented)", "(adapt| not-implemented)")
      
      
   def explainAdaptEvent(ev: Event with AdaptEvent): (String, String) = {
      ev match {
        case e:AdaptStart =>
          val short = if (e.pt == WildcardType) "Adapt expression \nwith no expected type" 
                      else "Adapt to the expected type" + safeTypePrint(e.pt, ":\n", "", truncate=false)
          val long = "Adapt expression's type (if necessary) to the expected type.\n" + 
                     "Found:    " + anyString(e.tree.tpe) + "\n" + 
                     "Expected: " + anyString(e.pt)
          (short, long)
          
        case e:AdaptDone =>
          DEFAULT

        case e:AdaptAnnotationsAdapt =>
          DEFAULT
          
        case e:ConstantTpeAdapt =>
          ("Adapt constant value", "")
          
        case e:OverloadedTpeAdapt =>
          DEFAULT
          
        case e:PolyTpeEmptyAdapt =>
          ("Adapt nullary method type", "")
          
        case e:ByNameParamClassAdapt =>
          DEFAULT
          
        case e:SkolemizeTpe1Adapt =>
          DEFAULT
          
        case e:SkolemizeTpe2Adapt =>
          DEFAULT
         
        case e:PolyTpeAdapt =>
          ("Adapt polymorphic type", 
           "\nType-parameters: '" + anyString(e.tparams) + "' " + 
           "\nResult type: '" + anyString(e.tpe) + "'" +
           "\nFor type tree: " + anyString(e.typeTree) + " undetermined context type-parameters: " + e.undetTParams)

        case e:ImplicitMethodTpeAdapt =>
          ("Adapt expression with method type\n and implicit parameter(s)",
           "Adapt expression \n" + anyString(e.tree) + "\n" +
           "having method type and implicit parameters " + anyString(e.tree.tpe) + "\n" +
           "Needs to find implicit argument in the context and apply it")
           
        case e:UndetParamsMethodTpeAdapt =>
          ("Infer expression instance\n for implicit method type", "")
          
        case e:SuccessSilentMethodTpeAdapt =>
          ("Successfully typed application\n of (inferred) implicit arguments",
           "Typed successfully application of inferred arguments for \n" +
           anyString(e.tree) + "\n" + "with type " + anyString(e.tpe))
           
        case e:InferImplicitForParamAdapt =>
          ("Infer implicit for parameter " + safeTypePrint(e.param.tpe, "\nof type: ", ""), 
           "Infer implicit for parameter '" + anyString(e.param) + "': " + anyString(e.param.tpe))
           
        case e:InferDivergentImplicitValueNotFound =>
          ("Implicit value not found",
           "Could not find implicit value for evidence parameter \n" + e.param.name + 
           "\nof type " + anyString(e.param.tpe))
           
        case e:InferImplicitValueNotFound =>
          ("Implicit value not found",
           "Could not find implicit value for evidence parameter \n" + e.param.name + 
           "\nof type " + anyString(e.param.tpe))
           
        case e:InferredImplicitAdapt =>
          ("Finished applying inferred \n implicit argument (if any)", "")
          
        case e:EtaMethodTpeAdapt =>
          ("Eta expansion",
           "Eta expansion of tree " + anyString(e.tree) +
           " with expected type: " + anyString(e.pt))
           
        case e:TreeAfterEtaExpansionMethodTpeAdapt =>
          ("Eta-expanded tree",
           anyString(e.tree) + "\nhas been eta-expanded to\n " + anyString(e.tree1))
           
        case e:InstantiateTParamsForEtaExpansionAdapt =>
          ("Instantiate type-parameters \n in eta expansion",
           anyString(e.tree) + " with type " + anyString(e.tree.tpe) +
           "\nTree's symbol " + anyString(e.meth) + " with type parameters '" +
           e.tparams.map(anyString) + "' to instantiate")
           
        case e:InferExprFailed =>
          ("Failed to infer expression instance",
           anyString(e.tree) + " with expected type " +
           anyString(e.pt) + "\nFailed with type error " + e.e)
           
        case e:ApplyNullaryMethodAdapt =>
          ("Apply nullary method",
           anyString(e.tree) + ": " + anyString(e.methTpe))
           
        case e:TypeTreeAdapt =>
          ("Adapt type tree", "")

        case e:FunModeAdapt =>
          DEFAULT
          
        case e:ConvertToTypeTreeAdapt =>
          DEFAULT // internal
         
        case e:PatternConstructorsAdapt =>
          ("Adapt pattern constructor", "")
          
        case e:ApplyAdapt =>
          ("Adapt expression to contain \n apply() member", "")
          
        case e:AdaptToNameQualAdapt =>
          DEFAULT
          
        case e:InferInstanceAdapt =>
          val short = e.e match {
             case DefaultExplanation =>
               "Adapt by inferring concrete instance"
             case _ =>
               "Infer concrete instance" //and \n " + explainNamer(e.e)
          }
          val long =
            "Instantiate undetermined paratemers " + e.tparams.map(anyString).mkString("[", ",", "]") + "\n" +
            (e.e match {
              case DefaultExplanation =>
                " and adapt by inferring concrete instance"
              case _ =>
                e.e // TODO
             }) + "\n" +
           "in expression \n" + anyString(e.tree) + "\n" + 
           "with expected type: " + anyString(e.pt)
         (short, long)

        case e:SuccessSubTypeAdapt =>
          ("Subtype constraint satisfied",
           "Constraint satisfied \n" + anyString(e.value1) +
           " <:< " + anyString(e.value2) + "\n in tree " + anyString(e.tree))
           
        case e:ConstantFoldSubTypeAdapt =>
          ("Subtype constraint for constants\n satisfied", "")
          
        case e:NotASubtypeAdapt => 
          ("Adapt expression's type to satisfy\n" +
           safeTypePrint(e.tpe, truncate=false) + " <: " + safeTypePrint(e.pt, truncate=false), 
           "FAILED subtype constraint:\n " +
           anyString(e.tpe) + " <: " + anyString(e.pt))

        case e:AdaptToUnitAdapt =>
          ("Adapt to Unit", "Adapt to Unit type \n " + anyString(e.tpe))
          
        case e:WeakConformanceAdapt =>
          ("Weak conformance adapt\n (numeric values)", "")
          
        case e:AnnotationCheckerAdapt =>
          DEFAULT
          
        case e:InstantiateAdapt =>
          ("Instantiate undetermined parameters", "")
          
        case e:FindViewAdapt =>
          ("Search for view that satisfies\n subtype constraint",
           "Find view that adapts initial expression's type to the type\n" +
           "that conforms to the expected type")
          
        case e:ApplyViewAdapt =>
          ("Apply (found) applicable view", 
           "Found view \n " + anyString(e.coercion) + "\n" +
           "which is applicable for the original argument")
           
        case e:NoViewFound =>
          ("No applicable view was found",
           "No view was found for tree\n" + anyString(e.tree) + "\n" +
           "of type " + anyString(e.tree.tpe) +
           " that conforms to the expected type " + anyString(e.pt))
        case _ =>
          DEFAULT
      }
    }
      
      
  }
}