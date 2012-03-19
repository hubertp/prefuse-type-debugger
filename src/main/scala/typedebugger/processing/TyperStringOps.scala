package scala.typedebugger
package processing

trait TyperStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
    
  trait TyperEventsOps {
    // TODO cache results
    
    private val DEFAULT = ("(typer | not implemented)", "(typer | not implemented)")
    
    def explainTyperEvent(ev: Event)(implicit time: Clock = ev.time): (String, String) = {
      ev match {
        case e: TyperTyped =>
          val treeToString: String = (if (e.tree.symbol != NoSymbol && e.tree.symbol != null) " " + e.tree.symbol else "")
          val long = "Typecheck tree\n" +
            snapshotAnyString(e.tree) + "\n" +
            "with expected type " + snapshotAnyString(e.pt) + "\n" +
            e.expl
 
          
          ("Typecheck " + treeToString, long)
        case e: TyperTyped1 =>
         ("Type tree", "Type tree " + snapshotAnyString(e.tree) + "\nwith expected type: " + snapshotAnyString(e.pt))
         
        case e: SymInitializeTyper =>
          ("Initialize tree's symbol type", "Initializing symbol will force its lazy type to be resolved")
          
        case e: PackageTyper =>
          ("Type package", "Type package " + e.tree.asInstanceOf[RefTree].name.toString)
          
        case e: ClassTyper =>
          ("Type class", "Type class " + snapshotAnyString(e.tree))
        
        case e: ModuleTyper =>
          ("Type object", "Type object " + snapshotAnyString(e.tree))
          
        case e: InitializedCompanionClassConstrs =>
          ("Initialize companion class\nconstructor", "Initialize linked class " + (if (e.linked == NoSymbol) "(none)" else e.linked))
          
        case e: ParentTypesTyper =>
          ("Determine parents' types", "Parents: " + e.parents.map(snapshotAnyString))
        
        case e: TypeInitialSuperTpeTyper =>
          val t = treeAt(e.tree)
          ("Type first parent\n as supertype",
           "Preliminary super-type: " + anyString(t) + " with type " + snapshotAnyString(t.tpe))
           
        case e: NewSuperTpePossiblyClass =>
          ("Replace non-class supertype", "Replace current non-class " +
          "supertype " + snapshotAnyString(e.supertpt) + " with its first parent " +
          snapshotAnyString(e.supertpt) + " (maybe a class). Also add old supertype to mixins.")
       
        case e: SuperTpeToPolyTpeTypeConstr =>
          ("Polymorphic super-type", "Polymorphic super type: " + snapshotAnyString(e.newSupertpt))
        
        case e: ConvertConstrBody =>
          ("Convert constructor", "Convert constructor body\n" + snapshotAnyString(e.tree))
          
        case e: ValidateParentClass =>
          ("Validate parent class", "Validate parent class: " + snapshotAnyString(e.parent))
          
        case e: ValidateSelfTpeSubtypingTyper =>
          val detailed = "Self-type: " + snapshotAnyString(e.selfTpe) + ", " + e.selfTpe.getClass + 
          "\nParent type: " + snapshotAnyString(e.parentTpe) + ", " + e.parentTpe.getClass + 
          "\nSubtypes: " + e.subtypingRes
          ("Self-type is a subtype of parent type", detailed)
          
        case e: ValDefTyper =>
          val tree = treeAt(e.valdef)
          val sym = SymbolSnapshot(tree.symbol)
          ("Type value definition" + safeTypePrint(sym.tpe, "\n(typed as ", ")"), "Type value definition \n" + anyString(tree))
          
        case e: DefDefTyper =>
          ("Type definition", "Type definition \n" + snapshotAnyString(e.tree))
          
        case e: TypeDefTyper =>
          ("Type type definition", "Type type definiton \n" + snapshotAnyString(e.tree))

///--------------------------------------
///--------------------------------------

        case e: LabelDefTyper =>
          ("Type label", "Type label " + snapshotAnyString(e.tree))
              
        case e: DocDefTyper =>
          ("Type documentation definition", "Documentation " + snapshotAnyString(e.tree))


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
          (short, snapshotAnyString(e.tree))

        case e: AssignTyper =>
          ("Type assignment", 
          "Type assignment of \n" + snapshotAnyString(e.value1) + "\nto " + snapshotAnyString(e.value2))
          
        case e: AssignLeftTyper =>
          ("Type left side of the assignment", "Type left side of the assignment of symbol " + snapshotAnyString(e.treeSym))

        case e: AssignGetterTyper =>
          ("Type getter for left side of the assignment", "")
    
        case e: AssignRightTyper =>
          ("Type right side of the assignment", "")
          
        case e: IfTyper =>
          ("Type if conditional", "")
    
        case e:IfCondTyper =>
          ("Type condition", "")
          
        case e:IfBranchTyper =>
          ("Type " + (if (e.cond) "then" else "else") + " branch", "With expected type: " + snapshotAnyString(e.pt))
          
        case e:IfLubTyper =>
          ("Determine least-upper-bound for if conditional", 
          snapshotAnyString(e.tree1) + ": " + snapshotAnyString(e.value1) + " and " +
          snapshotAnyString(e.tree2) + ": " + snapshotAnyString(e.value2) + "\n" +
          "with expected type: " + snapshotAnyString(e.pt))
          
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
          ("Type new instance", snapshotAnyString(e.tree))

        case e:NewTypeCtorTyper =>
          DEFAULT
          
        case e:NewTypeCtorWithParamsTyper =>
          ("Type constructor with type parameters",
           "Type parameters: " + e.params.map(snapshotAnyString).mkString(","))
           
        case e:EtaTyper =>
          DEFAULT
          
        case e:EtaByNameParamTyper =>
          DEFAULT
          
        case e:EtaEmptyPolyTyper =>
          DEFAULT
          
        case e:EtaPolyDoneTyper =>
          DEFAULT
    
        case e:EtaMethodTypeTyper =>
          val t = treeAt(e.tree)
          
          ("Perform eta-expansion adaption for method",
           "Eta-expand " + anyString(t) + "\n" + 
           "with type " + snapshotAnyString(t.tpe) + "\n" +
           "and params: " + e.params.map(snapshotAnyString))
           
        case e:TryTypedArgsTyper =>
          ("Typecheck arguments individually first", "")
          
        case e:TryTypedApplyTyper =>
          val t = treeAt(e.tree)
          val long = "Typecheck application of function to the arguments.\n" +
                     "If that fails adapt function to the arguments.\n" +
                     "Function: " + anyString(t) + "\n" +
                     "of type: " + snapshotAnyString(t.tpe) + "\n" +
                     "arguments: " + e.args.mkString("(", ",", ")") + "\n" + 
                     "with expected type " + snapshotAnyString(e.pt)
          ("Try typechecking application \n of function to arguments", long)

        case e:SuccessTryTypedApplyTyper =>
          ("Typed application", "")
          
        case e:SecondTryTypedApplyStartTyper =>
          val long = "Since first adapt at simply typing application of function\n" +
                     snapshotAnyString(e.fun) + "\n to arguments " +
                     e.args.map(snapshotAnyString).mkString("'", ",", "'") + "\n" +
                     "failed, try adapting function to the arguments"
 
          ("Second attempt: \n " +
          "Try adapting function to the given arguments", long)

        case e:SecondTryTypedApplyTyper =>
          ("Second attempt: \nType application of adapted function to arguments",
           "Second attempt at applying adapted function  '" + snapshotAnyString(e.qual1) +
           "' to arguments '" + snapshotAnyString(e.args1) + "'")

        case e:FailedSecondTryTypedApplyTyper =>
          ("Failed adapting function to the arguments",
           (if (e.args1 eq null) "Failed typing arguments '"
            else "Failed adapting qualifier to arguments '") + e.args1.map(snapshotAnyString).mkString("[", ",", "]") + "'")

        case e:FailedTryTypedApplyTyper =>
          ("Second attempt failed", "")
          
        case e:TypedWildcardTyper =>
          DEFAULT
          
        case e:TypedEtaTyper =>
          val expr  = treeAt(e.expr)
          val exprTpe = TypeSnapshot(expr.tpe)
          ("Type expression and \n eta-expand it", 
           "Eta-expansion on expression " + anyString(expr) + 
             (if (exprTpe != null) "\n of type " + anyString(exprTpe) else ""))
        
        case e: TypedTypedExprTyper =>
          ("Type explicitly typed expression",
           "Expression to be typed: " + snapshotAnyString(e.tree))
    
        case e: TypeApplyTyper =>
          ("Type type application", 
           "Type type application." + 
           "\nApply types: " + e.args.map(snapshotAnyString) + 
           "\n to type constructor " + snapshotAnyString(e.fun) + 
           "\n with expected type: " + snapshotAnyString(e.pt))
           
        case e:TypedTypeApplySuccessTyper =>
          ("Typechecked type application",
           "Typechecked type application: " + e.args.map(snapshotAnyString) +
           " applied to " + snapshotAnyString(e.fun) +
           " as " + snapshotAnyString(e.resultTpe))
           
        case e:TypedApplyStableTyper =>
          DEFAULT
        
        case e:TypedApplyUnstableTyper =>
          DEFAULT
        
        case e:SuccessTypedApplyFunTyper =>
          ("Successfully typed function as\n " + snapshotAnyString(e.expectedFunPt),
           "Function \n" + snapshotAnyString(e.tree) + " \n" +
           "was typed as \n" + snapshotAnyString(e.expectedFunPt) + "\n" +
           "in the context of expected type " + snapshotAnyString(e.pt))
          
        case e:TypedApplyToAssignment =>
          DEFAULT
          
        case e:ApplyBlockTyper =>
          ("Type block application", "")
          
        case e:ApplyTyper =>
          val app1 = treeAt(e.app)
          val long = "Type application of function \n" +
                     snapshotAnyString(app1.fun) + "\n" +
                     "to arguments " + app1.args.map(a => {val a0 = treeAt(a); anyString(a0) + ":" + snapshotAnyString(a0.tpe)}).mkString("(", ",", ")") +
                     (e.e match {
                        case DefaultExplanation => ""
                        case _ => "\n" + e.toString + (if (settings.debugTD.value) " expl: " + e.e.getClass else "")
                     })
          ("Type application", long)

        case e:ApplyDynamicTyper =>
          DEFAULT
          
        case e:SuperTyper =>
          ("Type super", "")
          
        case e:ThisTyper =>
          ("Type 'this'", "")

        case e:SelectTyper =>
          val qual1 = treeAt(e.qual)
          val long =
            "Type selection of \n" +
            snapshotAnyString(e.qual) + ".'" + e.name + "'\n" +
            "with qualifier of type \n" + snapshotAnyString(qual1.tpe) + "\n" +
            "and expected type " + snapshotAnyString(e.pt)
          ("Type member selection \n given correct qualifier", long)
          
        case e:SelectTreeTyper =>
         ("Type member selection",
          "Type qualifier \n" + snapshotAnyString(e.tree.asInstanceOf[Select].qualifier) + "\ncalling member '" + e.tree.asInstanceOf[Select].name + "'")
    
        case e:SelectConstrTyper =>
         ("Type super-constructor call", "")
         
        case e:TreeSymSelectTyper =>
          DEFAULT
    
        case e:SymSelectTyper =>
          val sym1 = SymbolSnapshot(e.sym)
          val short = (if (e.sym == NoSymbol) "No symbol found corresponding to\n the member qualifier" else "Found symbol corresponding to the member \nof the qualifier")
          val long = 
            "Result of finding symbol corresponding to the member '" + snapshotAnyString(e.member) + "' of qualifier: \n" +
            snapshotAnyString(e.qual) + "\n" +
            (if (e.sym == NoSymbol) "Symbol not found "
            else (" Found symbol " + anyString(sym1) + "\n with type " + snapshotAnyString(sym1.tpe)))
         (short, long)
         
        case e:SymExistsSelectTyper =>
          ("Valid symbol", "Valid symbol " + snapshotAnyString(e.sym))

        case e:StabilizeTreeTyper =>
          DEFAULT
          
        case e:DeferredTypeTreeTyper =>
          DEFAULT

        case e:IdentTyper =>
          ("Type identifier: " + e.tree.asInstanceOf[Ident].name + safeTypePrint(e.tree.symbol.tpe, "\n(typed as ", ")"), "")
          
        case e:LiteralTyper =>
          val tree1 = treeAt(e.tree)
          ("Type literal:\n" + anyString(tree1),
           "Typing " + e.tree.asInstanceOf[Literal].value + (if (tree1.tpe != null) "of type " + snapshotAnyString(tree1.tpe) else "") + "\n" +
           "with expected type " + snapshotAnyString(e.pt))
           
        case e:SingletonTypeTreeTyper =>
          DEFAULT
          
        case e:SelectFromTypeTreeTyper =>
          DEFAULT
          
        case e:CompoundTypeTyper =>
          DEFAULT
          
        case e:AppliedTypeTyper =>
          ("Type applied type",
           "Type applied type \n" + snapshotAnyString(e.tree) + "\n" +
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
            snapshotAnyString(e.templ) + " \n with parents: " +
            e.parents.map(p => { val p0 = treeAt(p); anyString(p0) + ": " + p0.tpe}).mkString(",") + " for " + snapshotAnyString(e.clazz)
          (short, long)

        case e:SelfTpeRefinedType =>
          DEFAULT
          
        // TODO: only for debugging purposes?
        case e:SelfTpeThis =>
          DEFAULT
          
        case e:AdaptToArgumentsTyper =>
          ("Adapt function to arguments",
           "Adapt qualifier " + snapshotAnyString(e.tree) + "\n" +
           "so that it contains a function " + e.name + "\n" +
           "that applies to arguments " + e.args.map(snapshotAnyString).mkString("(", ",", ")") + "\n" +
           "with the expected type " + snapshotAnyString(e.pt))

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
           "Infer view which adapts current tree " + snapshotAnyString(e.tree) + "\n" +
           "of type " + snapshotAnyString(e.tpe) + "\n" +
           "to the template type (enforced by member) " + snapshotAnyString(e.searchTpe))
           
        case e:OpenExistentialAdaptToMemberTyper => 
          DEFAULT
          
        case e:FoundCoercionAdapToMemberTyper =>
          ("Found coercion", 
           "Found coercion \n" + snapshotAnyString(e.coercion) + "\n" +
           "that adapts qualifier to the expected type")
           
        case e:FailedAdaptToMemberTyper =>
          ("View inference for adaptation \n FAILED", 
           "Failed inference of a view that adapts the tree \n" +
           snapshotAnyString(e.tree) + "\n" +
           "of type " + snapshotAnyString(e.tpe) + "\n" +
           "to type " + snapshotAnyString(e.searchTpe))
        
        case e:IsNotAdaptableTyper =>
          ("Qualifier is not adaptable \n FAILED",
           "Qualified " + snapshotAnyString(e.tree) + " with type " + snapshotAnyString(e.tpe) + " is not adaptable")
           
        case e:InferViewAdaptToMemberTyper =>
          ("Infer view that adapts \n qualifier to member",
           "Infer view which adapts tree.\n" +
           "Current type:  " + snapshotAnyString(e.value1) + "\n" +
           "Expected type: " + snapshotAnyString(e.value2))
           
        case e:DoTypedApplyTyper =>
          def argsToString(args0: List[Tree]) = args0.map(a => { val a0 = treeAt(a)
            anyString(a0) + ": " + (if (a0.tpe == null) "?" else snapshotAnyString(a0.tpe))
           }).mkString("(", ",", ")")

          ("Typecheck application of\n function to arguments", 
           "Typecheck application of function: '" + snapshotAnyString(e.fun) + "'" + 
           "\nto arguments '" + argsToString(e.args) + "'" + 
           "\nwith expected type: " + snapshotAnyString(e.pt))
           
        case e:OverloadedSymDoTypedApply =>
          ("Quick alternatives filter\n for overloaded function",
           "[Compiler optimization]\n Quickly filter-out alternatives for " +
           snapshotAnyString(e.sym) + "\n" +
           "and argument types " + e.argTypes.map(snapshotAnyString).mkString(","))

        case e:CheckApplicabilityAlternativeDoTypedApply =>
          ("Verify alternative symbol", 
           "Verifying applicability of the " + snapshotAnyString(e.alt) + " alternative " +
           "with type " + snapshotAnyString(e.alt.tpe) + "\n" +
           "for symbol " + snapshotAnyString(e.funSym) + " in the context of type " + snapshotAnyString(e.pt))

        case e:IsApplicableAlternativeDoTypedApply =>
          val isApplicable = if (e.applicable) "applicable" else "not applicable"
          ("Alternative is " + isApplicable, 
           "Alternative for " + snapshotAnyString(e.sym) + " with type " + snapshotAnyString(e.ftpe) + "\n" + 
           "is " + isApplicable)
           
        case e:FilteredDoTypedApply =>
          val funSym1 = SymbolSnapshot(e.funSym)
          ("Filtered-out alternatives", 
           "Initial filter out for symbol alernatives for tree " + snapshotAnyString(e.tree) +
           " results in symbol " + anyString(funSym1) + " of type " + snapshotAnyString(funSym1.tpe))
           
        case e:OverloadedTpeDoTypedApply =>
          ("Typecheck application \n for overloaded method",
           "Typechecking application of " + snapshotAnyString(e.fun) + " for overloaded method")

        case e:InferMethodAlternativeDoTypedApply =>
          ("Infer correct method \n for application \n from alternatives", 
           "")
    
        case e:AdaptInferredMethodAlternativeDoTypedApply =>
          val fun1 = treeAt(e.fun)
          ("Adapt inferred method alternative",             
           "Adapt inferred " + anyString(fun1) + " of type " + snapshotAnyString(fun1.tpe)) 
        
        case e:InferredMethodDoTypedApply =>
          val fun1 = treeAt(e.fun)
          ("Typecheck arguments application \nfor the inferred method",
           "Do typed apply on an inferred and adapted method " + anyString(fun1) + " of type " + snapshotAnyString(fun1.tpe)) 
           
        case e:MethodTpeDoTypedApply =>
          ("Function with Method Type", 
           "parameter symbols: '" + e.params.map(snapshotAnyString) +
           "' and their types: '" + e.params.map(p => combinedSnapshotAnyString(p)(_.tpe)) + "'")

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
           "\n" + snapshotAnyString(e.tree) +
           "\nFormals: " + e.formals.map(snapshotAnyString).mkString("(", ",", ")") +
           "\nArguments " + e.args.map(snapshotAnyString).mkString("(", ",", ")"))

        case e:TParamsResolvedDoTypedApply =>
          ("All type parameters are resolved",
           "All type parameters (if any) in typed application were resolved for tree:" +
           snapshotAnyString(e.tree))

        case e:ApplyTreeDoneDoTypedApply =>
          val tree1 = treeAt(e.tree)
          ("Successfully typed application",
           "Type application in \n" + anyString(tree1) + "\n" +
           "as " + snapshotAnyString(tree1.tpe))
           
        case e:NeedsInstantiationDoTypedApply =>
          ("Method Type needs instantiation", "")
          
        case e:MethodTpeWithUndetTpeParamsDoTypedApply =>
          ("Method Type with undetermined type parameters.\nNeed to resolve them first.",
           "Expression \n" + snapshotAnyString(e.tree) + "\n" +
           "with Method Type\n with undetermined type parameters: '" + e.tparams.map(snapshotAnyString).mkString(",") + "'\n" +
           "and formal parameters: " + e.formals.map(snapshotAnyString).mkString("[", ",", "]"))    

        case e:ProtoTypeArgsDoTypedApply =>
          ("Infer prototype arguments", 
           "Infer prototype arguments:\n" +
           e.tparams.map(snapshotAnyString).mkString("[", ",", "]") + " for " + snapshotAnyString(e.resultTpe))
          
        case e:InstantiatedDoTypedApply =>
          val fun1 = treeAt(e.fun)
          ("Typecheck inferred instance" + safeTypePrint(fun1.tpe, "\n", "\n") + " in the application",
           "Typecheck inferred instance for \n" +
           snapshotAnyString(e.fun) + "\nwith type " + snapshotAnyString(fun1.tpe) +
           (if (!e.undet.isEmpty) "\n and still undetermined type parameters " +
               e.undet.map(snapshotAnyString).mkString(",") else "") +
           "\n and expected type " + snapshotAnyString(e.pt))
           
        case e:DoTypedApplyDone =>
          // TODO: should be errorevent somehow?
          val short = if (e.tree.isErrorTyped) "Failed to type application" else "Typechecked application"
          val tree1 = treeAt(e.tree)
          (short,
           "Applied arguments in the tree \n " + snapshotAnyString(tree1) + "\n" + 
           "of type " + snapshotAnyString(tree1.tpe))

        case e:SingleTpeDoTypedApply =>
          DEFAULT
          
        case e:UnapplyDoTypedApply =>
          DEFAULT
          
        case _ => (ev.lName, ev.toString)
      }
    }
  }
}