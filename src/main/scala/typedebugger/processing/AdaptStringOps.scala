package scala.typedebugger
package processing

trait AdaptStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  
  trait AdaptEventsOps {
    private val DEFAULT = ("(adapt| not-implemented)", "(adapt| not-implemented)")
      
      
   def explainAdaptEvent(ev: Event with AdaptEvent)(implicit time: Clock = ev.time): (String, String) = {
      ev match {
        case e:AdaptStart =>
          val short = if (e.pt == WildcardType) "Adapt expression \nwith no expected type" 
                      else "Adapt to the expected type" + safeTypePrint(e.pt, ":\n", "", truncate=false)
          val tree1 = treeAt(e.tree)
          val long = "Adapt expression's type (if necessary) to the expected type.\n" + 
                     "Found:    " + snapshotAnyString(tree1.tpe) + "\n" + 
                     "Expected: " + snapshotAnyString(e.pt)
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
           "\nType-parameters: '" + snapshotAnyString(e.tparams) + "' " + 
           "\nResult type: '" + snapshotAnyString(e.tpe) + "'" +
           "\nFor type tree: " + snapshotAnyString(e.typeTree) + " undetermined context type-parameters: " + e.undetTParams)

        case e:ImplicitMethodTpeAdapt =>
          val tree1 = treeAt(e.tree)
          ("Adapt expression with method type\n and implicit parameter(s)",
           "Adapt expression \n" + anyString(tree1) + "\n" +
           "having method type and implicit parameters " + snapshotAnyString(tree1.tpe) + "\n" +
           "Needs to find implicit argument in the context and apply it")
           
        case e:UndetParamsMethodTpeAdapt =>
          ("Infer expression instance\n for implicit method type", "")
          
        case e:SuccessSilentMethodTpeAdapt =>
          ("Successfully typed application\n of (inferred) implicit arguments",
           "Typed successfully application of inferred arguments for \n" +
           snapshotAnyString(e.tree) + "\n" + "with type " + snapshotAnyString(e.tpe))
           
        case e:InferImplicitForParamAdapt =>
          val param1 = SymbolSnapshot(e.param)
          ("Infer implicit for parameter " + safeTypePrint(param1.tpe, "\nof type: ", ""), 
           "Infer implicit for parameter '" + anyString(param1) + "': " + snapshotAnyString(param1.tpe))
           
        case e:InferDivergentImplicitValueNotFound =>
          val param1 = SymbolSnapshot(e.param)
          ("Implicit value not found",
           "Could not find implicit value for evidence parameter \n" + param1.name + 
           "\nof type " + snapshotAnyString(param1.tpe))
           
        case e:InferImplicitValueNotFound =>
          val param1 = SymbolSnapshot(e.param)
          ("Implicit value not found",
           "Could not find implicit value for evidence parameter \n" + param1.name + 
           "\nof type " + snapshotAnyString(param1.tpe))
           
        case e:InferredImplicitAdapt =>
          ("Finished applying inferred \n implicit argument (if any)", "")
          
        case e:EtaMethodTpeAdapt =>
          ("Eta expansion",
           "Eta expansion of tree " + snapshotAnyString(e.tree) +
           " with expected type: " + snapshotAnyString(e.pt))
           
        case e:TreeAfterEtaExpansionMethodTpeAdapt =>
          ("Eta-expanded tree",
           snapshotAnyString(e.tree) + "\nhas been eta-expanded to\n " + snapshotAnyString(e.tree1))
           
        case e:InstantiateTParamsForEtaExpansionAdapt =>
          val tree1 = treeAt(e.tree)
          ("Instantiate type-parameters \n in eta expansion",
           anyString(tree1) + " with type " + snapshotAnyString(tree1.tpe) +
           "\nTree's symbol " + snapshotAnyString(e.meth) + " with type parameters '" +
           e.tparams.map(snapshotAnyString) + "' to instantiate")
           
        case e:InferExprFailed =>
          ("Failed to infer expression instance",
           snapshotAnyString(e.tree) + " with expected type " +
           snapshotAnyString(e.pt) + "\nFailed with type error " + e.e)
           
        case e:ApplyNullaryMethodAdapt =>
          ("Apply nullary method",
           snapshotAnyString(e.tree) + ": " + snapshotAnyString(e.methTpe))
           
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
            "Instantiate undetermined paratemers " + e.tparams.map(snapshotAnyString).mkString("[", ",", "]") + "\n" +
            (e.e match {
              case DefaultExplanation =>
                " and adapt by inferring concrete instance"
              case _ =>
                e.e // TODO
             }) + "\n" +
           "in expression \n" + snapshotAnyString(e.tree) + "\n" + 
           "with expected type: " + snapshotAnyString(e.pt)
         (short, long)

        case e:SuccessSubTypeAdapt =>
          ("Subtype constraint satisfied",
           "Constraint satisfied \n" + snapshotAnyString(e.value1) +
           " <:< " + snapshotAnyString(e.value2) + "\n in tree " + snapshotAnyString(e.tree))
           
        case e:ConstantFoldSubTypeAdapt =>
          ("Subtype constraint for constants\n satisfied", "")
          
        case e:NotASubtypeAdapt => 
          ("Adapt expression's type to satisfy\n" +
           safeTypePrint(e.tpe, truncate=false) + " <: " + safeTypePrint(e.pt, truncate=false), 
           "FAILED subtype constraint:\n " +
           snapshotAnyString(e.tpe) + " <: " + snapshotAnyString(e.pt))

        case e:AdaptToUnitAdapt =>
          ("Adapt to Unit", "Adapt to Unit type \n " + snapshotAnyString(e.tpe))
          
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
           "Found view \n " + snapshotAnyString(e.coercion) + "\n" +
           "which is applicable for the original argument")
           
        case e:NoViewFound =>
          ("No applicable view was found",
           "No view was found for tree\n" + snapshotAnyString(e.tree) + "\n" +
           "of type " + snapshotAnyString(e.tree.tpe) +
           " that conforms to the expected type " + snapshotAnyString(e.pt))
        case _ =>
          DEFAULT
      }
    }
  }
}