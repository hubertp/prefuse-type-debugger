package scala.typedebugger
package processing

trait TyperStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
    
  trait TyperEventsOps {
    // TODO cache results
    
    private val DEFAULT = ("(typer | not implemented)", "(typer | not implemented)")
    
    def explainTyperEvent(ev: Event): (String, String) = {
      ev match {
        case e: TyperTyped =>
          val treeToString: String = (if (e.tree.symbol != NoSymbol && e.tree.symbol != null) " " + e.tree.symbol else "")
          val long = "Typecheck tree\n" +
            anyString(e.tree) + "\n" +
            "with expected type " + anyString(e.pt) + "\n" +
            e.expl
 
          
          ("Typecheck " + treeToString, long)
        case e: TyperTyped1 =>
         ("Type tree", "Type tree " + anyString(e.tree) + "\nwith expected type: " + anyString(e.pt))
         
        case e: SymInitializeTyper =>
          ("Initialize tree's symbol", "Initializing symbol will force its type to be resolved")
          
        case e: PackageTyper =>
          ("Type package", "Type package " + e.tree.asInstanceOf[RefTree].name.toString)
          
        case e: ClassTyper =>
          ("Type class", "Type class " + anyString(e.tree))
        
        case e: ModuleTyper =>
          ("Type object", "Type object " + anyString(e.tree))
          
        case e: InitializedCompanionClassConstrs =>
          ("Initialize companion class\nconstructor", "Initialize linked class " + (if (e.linked == NoSymbol) "(none)" else e.linked))
          
        case e: ParentTypesTyper =>
          ("Determine parents' types", "Parents: " + e.parents.map(anyString))
        
        case e: TypeInitialSuperTpeTyper =>
          ("Type first parent\n as supertype",
           "Preliminary super-type: " + anyString(e.tree) + " with type " + anyString(e.tree.tpe))
           
        case e: NewSuperTpePossiblyClass =>
          ("Replace non-class supertype", "Replace current non-class " +
          "supertype " + anyString(e.supertpt) + " with its first parent " +
          anyString(e.supertpt) + " (maybe a class). Also add old supertype to mixins.")
       
        case e: SuperTpeToPolyTpeTypeConstr =>
          ("Polymorphic super-type", "Polymorphic super type: " + anyString(e.newSupertpt))
        
        case e: ConvertConstrBody =>
          ("Convert constructor", "Convert constructor body\n" + anyString(e.tree))
          
        case e: ValidateParentClass =>
          ("Validate parent class", "Validate parent class: " + anyString(e.parent))
          
        case e: ValidateSelfTpeSubtypingTyper =>
          val detailed = "Self-type: " + anyString(e.selfTpe) + ", " + e.selfTpe.getClass + 
          "\nParent type: " + anyString(e.parentTpe) + ", " + e.parentTpe.getClass + 
          "\nSubtypes: " + e.subtypingRes
          ("Self-type is a subtype of parent type", detailed)
          
        case e: ValDefTyper =>
          ("Type value definition", "Type value definition \n" + anyString(e.tree))
          
        case e: DefDefTyper =>
          ("Type definition", "Type definition \n" + anyString(e.tree))
          
        case e: TypeDefTyper =>
          ("Type type definition", "Type type definiton \n" + anyString(e.tree))

///--------------------------------------
///--------------------------------------

        case e: LabelDefTyper =>
          ("Type label", "Type label " + anyString(e.tree))
              
        case e: DocDefTyper =>
          ("Type documentation definition", "Documentation " + anyString(e.tree))


        case e: AnnotatedTyper =>
          DEFAULT
          
        case e: BlockTyper =>
          ("Type block of statements", "")
          
        case e: AlternativeTyper =>
          DEFAULT
          
        case e: StarTyper =>
          DEFAULT


        case e: BindTyper =>
          DEFAULT
          
        case e: UnApplyTyper =>
          DEFAULT
          
        case e: ArrayValueTyper =>
          DEFAULT
          
        case e: FunctionTyper =>
          val short = if (e.constr) "Type constructor" else "Type function"
          (short, anyString(e.tree))

        case e: AssignTyper =>
          ("Type assignment", 
          "Type assignment of \n" + anyString(e.value1) + "\nto " + anyString(e.value2))
          
        case e: AssignLeftTyper =>
          ("Type left side of the assignment", "Type left side of the assignment of symbol " + anyString(e.treeSym))

        case e: AssignGetterTyper =>
          ("Type getter for left side of the assignment", "")
    
        case e: AssignRightTyper =>
          ("Type right side of the assignment", "")
          
        case e: IfTyper =>
          ("Type if conditional", "")
    
        case e:IfCondTyper =>
          ("Type condition", "")
          
        case e:IfBranchTyper =>
          ("Type " + (if (e.cond) "then" else "else") + " branch", "With expected type: " + anyString(e.pt))
          
        case e:IfLubTyper =>
          ("Determine least-upper-bound for if conditional", 
          anyString(e.tree1) + ": " + anyString(e.value1) + " and " +
          anyString(e.tree2) + ": " + anyString(e.value2) + "\n" +
          "with expected type: " + anyString(e.pt))
          
        case e:MatchTyper =>
          DEFAULT
          
        // Start typed Return
        case e:ReturnTyper =>
          DEFAULT
          
        case e:TryTyper =>
          DEFAULT
          
        case e:ThrowTyper =>
          DEFAULT
          
        case e:NewTyper =>
          ("Type new instance", anyString(e.tree))

        case e:NewTypeCtorTyper =>
          DEFAULT
          
        case e:NewTypeCtorWithParamsTyper =>
          ("Type constructor with type parameters",
           "Type parameters: " + e.params.map(anyString).mkString(","))
           
        case e:EtaTyper =>
          DEFAULT
          
        case e:EtaByNameParamTyper =>
          DEFAULT
          
        case e:EtaEmptyPolyTyper =>
          DEFAULT
          
        case e:EtaPolyDoneTyper =>
          DEFAULT
    
        case e:EtaMethodTypeTyper =>
          ("Perform eta-expansion adaption for method",
           "Eta-expand " + anyString(e.tree) + "\n" + 
           "with type " + anyString(e.tree.tpe) + "\n" +
           "and params: " + e.params.map(anyString))
           
        case e:TryTypedArgsTyper =>
          ("Typecheck arguments individually first", "")
          
        case e:TryTypedApplyTyper =>
          val long = "Typecheck application of function to the arguments.\n" +
                     "If that fails adapt function to the arguments.\n" +
                     "Function: " + anyString(e.tree) + "\n" +
                     "of type: " + anyString(e.tree.tpe) + "\n" +
                     "arguments: " + e.args.mkString("(", ",", ")") + "\n" + 
                     "with expected type " + anyString(e.pt)
          ("Try typechecking application \n of function to arguments", long)

        case e:SuccessTryTypedApplyTyper =>
          ("Successfully typed application", "")
          
        case e:SecondTryTypedApplyStartTyper =>
          val long = "Since first adapt at simply typing application of function\n" +
                     anyString(e.fun) + "\n to arguments " +
                     e.args.map(anyString).mkString("'", ",", "'") + "\n" +
                     "failed, try adapting function to the arguments"
 
          ("Second attempt: \n " +
          "Try adapting function to the given arguments", long)

        case e:SecondTryTypedApplyTyper =>
          ("Second attempt: \nType application of adapted function to arguments",
           "Second attempt at applying adapted function  '" + anyString(e.qual1) +
           "' to arguments '" + anyString(e.args1) + "'")

        case e:FailedSecondTryTypedApplyTyper =>
          ("Failed adapting function to the arguments",
           (if (e.args1 eq null) "Failed typing arguments '"
            else "Failed adapting qualifier to arguments '") + e.args1.map(anyString).mkString("[", ",", "]") + "'")

        case e:FailedTryTypedApplyTyper =>
          ("Second attempt failed", "")
          
        case e:TypedWildcardTyper =>
          DEFAULT
          
        case e:TypedEtaTyper =>
          ("Type expression and \n eta-expand it", 
           "Eta-expansion on expression " + anyString(e.expr) + 
             (if (e.expr.tpe != null) "\n of type " + anyString(e.expr.tpe) else ""))
        
        case e: TypedTypedExprTyper =>
          ("Type explicitly typed expression",
           "Expression to be typed: " + anyString(e.tree))
    
        case e: TypeApplyTyper =>
          ("Type type application", 
           "Type type application." + 
           "\nApply types: " + e.args.map(anyString) + 
           "\n to type constructor " + anyString(e.fun) + 
           "\n with expected type: " + anyString(e.pt))
           
        case e:TypedTypeApplySuccessTyper =>
          ("Typechecked type application",
           "Typechecked type application: " + e.args.map(anyString) +
           " applied to " + anyString(e.fun) +
           " as " + anyString(e.resultTpe))
           
        case e:TypedApplyStableTyper =>
          DEFAULT
        
        case e:TypedApplyUnstableTyper =>
          DEFAULT
        
        case e:SuccessTypedApplyFunTyper =>
          ("Successfully typed function as\n " + anyString(e.expectedFunPt),
           "Function \n" + anyString(e.tree) + " \n" +
           "was typed as \n" + anyString(e.expectedFunPt) + "\n" +
           "in the context of expected type " + anyString(e.pt))
          
        case e:TypedApplyToAssignment =>
          DEFAULT
          
        case e:ApplyBlockTyper =>
          ("Type block application", "")
          
        case e:ApplyTyper =>
          val long = "Type application of function \n" +
                     anyString(e.app.fun) + "\n" +
                     "to arguments " + e.app.args.map(a => anyString(a) + ":" + anyString(a.tpe)).mkString("(", ",", ")") +
                     (e.e match {
                        case DefaultExplanation => ""
                        case _ => "\n" + e.toString
                     })
          ("Type application", long)

        case e:ApplyDynamicTyper =>
          DEFAULT
          
        case e:SuperTyper =>
          ("Type super", "")
          
        case e:ThisTyper =>
          ("Type 'this'", "")

        case e:SelectTyper =>
          val long =
            "Type selection of \n" +
            anyString(e.qual) + ".'" + e.name + "'\n" +
            "with qualifier of type \n" + anyString(e.qual.tpe) + "\n" +
            "and expected type " + anyString(e.pt)
          ("Type member selection \n given correct qualifier", long)
          
        case e:SelectTreeTyper =>
         ("Type member selection", 
          "Type qualifier \n" + anyString(e.tree.asInstanceOf[Select].qualifier) + "\ncalling member '" + e.tree.asInstanceOf[Select].name + "'")
    
        case e:SelectConstrTyper =>
         ("Type super-constructor call", "")
         
        case e:TreeSymSelectTyper =>
          DEFAULT
    
        case e:SymSelectTyper =>
          val short = (if (e.sym == NoSymbol) "No symbol found corresponding to\n the member qualifier" else "Found symbol corresponding to the member \nof the qualifier")
          val long = 
            "Result of finding symbol corresponding to the member '" + anyString(e.member) + "' of qualifier: \n" +
            anyString(e.qual) + "\n" +
            (if (e.sym == NoSymbol) "Symbol not found "
            else (" Found symbol " + anyString(e.sym) + "\n with type " + anyString(e.sym.tpe)))
         (short, long)
         
        case e:SymExistsSelectTyper =>
          ("Valid symbol", "Valid symbol " + anyString(e.sym))

        case e:StabilizeTreeTyper =>
          DEFAULT
          
        case e:DeferredTypeTreeTyper =>
          DEFAULT

        case e:IdentTyper =>
          ("Type identifier:\n " + e.tree.asInstanceOf[Ident].name.toString, "")
          
        case e:LiteralTyper =>
          ("Type literal:\n" + anyString(e.tree),
           "Typing " + e.tree.asInstanceOf[Literal].value + (if (e.tree.tpe != null) "of type " + anyString(e.tree.tpe) else "") + "\n" +
           "with expected type " + anyString(e.pt))
           
        case e:SingletonTypeTreeTyper =>
          DEFAULT
          
        case e:SelectFromTypeTreeTyper =>
          DEFAULT
          
        case e:CompoundTypeTyper =>
          DEFAULT
          
        case e:AppliedTypeTyper =>
          ("Type applied type",
           "Type applied type \n" + anyString(e.tree) + "\n" +
           "with arguments " + e.args.mkString(","))
    
        // TODO: there seems to be a bug related to range positions, hence 
        // def foo[T] will not show correctly the position for T
        case e:TypeBoundsTyper =>
          ("Type type-bounds", "Bounds: " + e.bounds)

        case e:ExistentialTypeTyper =>
          DEFAULT

        case e:DeferredRefCheckTypeTyper =>
          DEFAULT
          
        case e:TypeTreeTyper =>
          DEFAULT
          
        case e:ImportTyper =>
          DEFAULT

 
        case e:UnexpectedTyper =>
          DEFAULT
          
        case e:TemplateTyper =>
          val short = e.info match {
            case ClassTemplate  => "Type class template"
            case ModuleTemplate => "Type object template"
            case TraitTemplate => "Type trait template"
          }

          val long =
            anyString(e.templ) + " \n with parents: " +
            e.parents.map(p => anyString(p) + ": " + p.tpe).mkString(",") + " for " + anyString(e.clazz)
          (short, long)

        case e:SelfTpeRefinedType =>
          DEFAULT
          
        // TODO: only for debugging purposes?
        case e:SelfTpeThis =>
          DEFAULT
          
        case e:AdaptToArgumentsTyper =>
          ("Adapt function to arguments",
           "Adapt qualifier " + anyString(e.tree) + "\n" +
           "so that it contains a function " + e.name + "\n" +
           "that applies to arguments " + e.args.map(anyString).mkString("(", ",", ")") + "\n" +
           "with the expected type " + anyString(e.pt))

        case e:FinishedAdaptToArgumentsTyper =>
          val short = 
            (if (e.value1 eq e.value2) "Failed all attempts to adapt function to arguments"
             else "Adapted function to the arguments")
          val long = 
            if (e.value1 eq e.value2)
              ("Failed all attempts to adapt qualifier \n" + e.value1)
            else
              ("Adapted qualifier \n" + e.value1 + "\n" + "with the transformation: \n" + e.value2)
          (short, long)
          
        case e:FallbackAdaptToArgumentsTyper =>
          ("Fallback: adapt qualifier\n without any expected type",
           "Adapt qualifier to the member but without any expected return type")
           
        case e:AdaptToMemberTyper =>
          ("Adapt to member",
           "Infer view which adapts current tree " + anyString(e.tree) + "\n" +
           "of type " + anyString(e.tpe) + "\n" +
           "to the template type (enforced by member) " + anyString(e.searchTpe))
           
        case e:OpenExistentialAdaptToMemberTyper => 
          DEFAULT
          
        case e:FoundCoercionAdapToMemberTyper =>
          ("Found coercion", 
           "Found coercion \n" + anyString(e.coercion) + "\n" +
           "that adapts qualifier to the expected type")
           
        case e:FailedAdaptToMemberTyper =>
          ("View inference for adaptation \n FAILED", 
           "Failed inference of a view that adapts the tree \n" +
           anyString(e.tree) + "\n" +
           "of type " + anyString(e.tpe) + "\n" +
           "to type " + anyString(e.searchTpe))
        
        case e:IsNotAdaptableTyper =>
          ("Qualifier is not adaptable \n FAILED",
           "Qualified " + anyString(e.tree) + " with type " + anyString(e.tpe) + " is not adaptable")
           
        case e:InferViewAdaptToMemberTyper =>
          ("Infer view that adapts \n qualifier to member",
           "Infer view which adapts tree.\n" +
           "Current type:  " + anyString(e.value1) + "\n" +
           "Expected type: " + anyString(e.value2))
           
        case e:DoTypedApplyTyper =>
          def argsToString(args0: List[Tree]) = args0.map(a =>
            anyString(a) + ": " + (if (a.tpe == null) "?" else anyString(a.tpe))
           ).mkString("(", ",", ")")

          ("Typecheck application of\n function to arguments", 
           "Typecheck application of function: '" + anyString(e.fun) + "'" + 
           "\nto arguments '" + argsToString(e.args) + "'" + 
           "\nwith expected type: " + anyString(e.pt))
           
        case e:OverloadedSymDoTypedApply =>
          ("Quick alternatives filter for overloaded-function",
           "[Compiler optimization]\n Quickly filter-out alternatives for " +
           anyString(e.sym) + "\n" +
           "and argument types " + e.argTypes.map(anyString).mkString(","))

        case e:CheckApplicabilityAlternativeDoTypedApply =>
          ("Verify alternative symbol", 
           "Verifying applicability of the " + anyString(e.alt) + " alternative " +
           "with type " + anyString(e.alt.tpe) + "\n" +
           "for symbol " + anyString(e.funSym) + " in the context of type " + anyString(e.pt))

        case e:IsApplicableAlternativeDoTypedApply =>
          val isApplicable = if (e.applicable) "applicable" else "not applicable"
          ("Alternative is " + isApplicable, 
           "Alternative for " + anyString(e.sym) + " with type " + anyString(e.ftpe) + "\n" + 
           "is " + isApplicable)
           
        case e:FilteredDoTypedApply =>
          ("Filtered-out alternatives", 
           "Initial filter out for symbol alernatives for tree " + anyString(e.tree) +
           " results in symbol " + anyString(e.funSym) + " of type " + anyString(e.funSym.tpe))
           
        case e:OverloadedTpeDoTypedApply =>
          ("Typecheck application \n for overloaded method",
           "Typechecking application of " + anyString(e.fun) + " for overloaded method")

        case e:InferMethodAlternativeDoTypedApply =>
          ("Infer correct method \n for application \n from alternatives", 
           "")
    
        case e:AdaptInferredMethodAlternativeDoTypedApply =>
          ("Adapt inferred method alternative",             
           "Adapt inferred " + anyString(e.fun) + " of type " + anyString(e.fun.tpe)) 
        
        case e:InferredMethodDoTypedApply =>
          ("Typecheck arguments application \nfor the inferred method",
           "Do typed apply on an inferred and adapted method " + anyString(e.fun) + " of type " + anyString(e.fun.tpe)) 
           
        case e:MethodTpeDoTypedApply =>
          ("Function with Method Type", 
           "parameter symbols: '" + e.params.map(anyString) +
           "' and their types: '" + e.params.map(p => anyString(p.tpe)) + "'")

        case e:TryTupleApplyDoTypedApply =>
          DEFAULT
          
        case e:PackArgsDoTypedApply =>
          DEFAULT
          
        case e:PackedArgsDoTypedApply =>
          DEFAULT
          
        case e:TryNamesDefaultsDoTypedApply =>
          DEFAULT
          
        case e:CorrectArgumentsDoTypedApply =>
          ("Number of arguments agrees \nwith the number of parameters",
           "Number of arguments agrees with formals in tree:" +
           "\n" + anyString(e.tree) +
           "\nFormals: " + e.formals.map(anyString).mkString("(", ",", ")") +
           "\nArguments " + e.args.map(anyString).mkString("(", ",", ")"))

        case e:TParamsResolvedDoTypedApply =>
          ("All type parameters are resolved",
           "All type parameters (if any) in typed application were resolved for tree:" +
           anyString(e.tree))

        case e:ApplyTreeDoneDoTypedApply =>
          ("Successfully typed application",
           "Type application in \n" + anyString(e.tree) + "\n" +
           "as " + anyString(e.tree.tpe))
           
        case e:NeedsInstantiationDoTypedApply =>
          ("Method Type needs instantiation", "")
          
        case e:MethodTpeWithUndetTpeParamsDoTypedApply =>
          ("Method Type with undetermined type parameters.\nNeed to resolve them first.",
           "Expression \n" + anyString(e.tree) + "\n" +
           "with Method Type\n with undetermined type parameters: '" + e.tparams.map(anyString).mkString(",") + "'\n" +
           "and formal parameters: " + e.formals.map(anyString).mkString("[", ",", "]"))    

        case e:ProtoTypeArgsDoTypedApply =>
          ("Infer prototype arguments", 
           "Infer prototype arguments:\n" +
           e.tparams.map(anyString).mkString("[", ",", "]") + " for " + anyString(e.resultTpe))
          
        case e:InstantiatedDoTypedApply =>
          ("Typecheck inferred instance\n in the application",
           "Typecheck inferred instance for \n" +
           anyString(e.fun) + "\nwith type " + anyString(e.fun.tpe) +
           (if (!e.undet.isEmpty) "\n and still undetermined type parameters " +
               e.undet.map(anyString).mkString(",") else "") +
           "\n and expected type " + anyString(e.pt))
           
        case e:DoTypedApplyDone =>
          val short = if (e.tree.isErrorTyped) "Failed to type application" else "Typechecked application"
          (short,
           "Applied arguments in the tree \n " + anyString(e.tree) + "\n" + 
           "of type " + anyString(e.tree.tpe))

        case e:SingleTpeDoTypedApply =>
          DEFAULT
          
        case e:UnapplyDoTypedApply =>
          DEFAULT
          
        case _ => (ev.lName, ev.toString)
      }
    }
  }
}