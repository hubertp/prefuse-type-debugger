package scala.typedebugger
package stringops


trait TyperStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  import util.StringFormatter._ // shouldn't implicit figure out it automatically
    
  trait TyperEventsOps {
    self: Descriptors =>
    
    private val DEFAULT = new DefaultDescriptor("typer")
    
    def explainTyperEvent(ev: Event)(implicit time: Clock = ev.time): Descriptor = ev match {
      case e: TyperTyped =>
        new Descriptor(){
          
          
          def basicInfo = {
            val treeToString: String = (if (e.tree.symbol != NoSymbol && e.tree.symbol != null) " " + e.tree.symbol else "")
            "Typecheck " + treeToString
          }
          
          def fullInfo = {
            val tree0 = treeAt(e.tree)
            val tpe0 = TypeSnapshot(tree0.tpe)
            val sym0 = if (tree0.symbol != null) SymbolSnapshot(tree0.symbol) else null
            val tpe = if (tpe0 != null) tpe0 else if (sym0 != null) TypeSnapshot(sym0.tpe) else null
            val tpeRes = if (e.tree.tpe != null) e.tree.tpe else (if (e.tree.symbol != null) e.tree.symbol.tpe else null)
            debug(if (tree0 != null) "Tree class " + tree0.getClass + " with sym " + sym0 else "", "event")
            (e.expl + "\n\n" +
             "Typechecking tree: %tree" +
             "Expected type: %tpe" +
             "\n" +
             "Type of tree at the entry point: %tpe" +
             "Result type of the tree: %tpe").dFormat(Some("Typecheck tree"),
             anyString(tree0), snapshotAnyString(e.pt), anyString(tpe), anyString(tpeRes))
          }
        }

      case e: TyperTyped1 =>
        new Descriptor() {
          def basicInfo = "Type tree"
          def fullInfo  = {
            "Type tree %tree" +
            "Expected type: %tpe".dFormat(Some("Type Tree"), snapshotAnyString(e.tree), snapshotAnyString(e.pt))
            //"Type tree " + snapshotAnyString(e.tree) +
            //"\nwith expected type: " + snapshotAnyString(e.pt)
          }
        }
       
      case e: SymInitializeTyper =>
        new Descriptor() {
          def basicInfo = "Initialize the type of the symbol of the tree"
          def fullInfo  = 
            "Initializing symbol will force its lazy type to be resolved".dFormat(Some("Initialize lazy type"))
        }

      case e: TyperOmittedStatement =>
        new Descriptor() {
          def basicInfo = "Typechecking omitted"
          def fullInfo  = "Statement %tree" +
                          "was not typechecked as targeted debugging was used.\n" +
                          "Statement was not on a direct path to the target selection".dFormat(Some("Omitted typechecking"),
                            anyString(e.tree))
        }
        
      case e: PackageTyper =>
        new Descriptor() {
          def basicInfo = "Type package"
          def fullInfo  = "Type package %tree".dFormat(e.tree.asInstanceOf[RefTree].name.toString)
        }
        
      case e: ClassTyper =>
        new Descriptor() {
          def basicInfo = "Type class"
          def fullInfo  = "Type class %tree".dFormat(snapshotAnyString(e.tree))
        }
      
      case e: ModuleTyper =>
        new Descriptor() {
          def basicInfo = "Type object"
          def fullInfo  = "Type object %tree".dFormat(snapshotAnyString(e.tree))
        }
        
      case e: InitializedCompanionClassConstrs =>
        new Descriptor() {
          def basicInfo = "Initialize companion class\nconstructor"
          def fullInfo  = "Initialize linked class " +
                          (if (e.linked == NoSymbol) "(none)" else e.linked)
        }
        
      case e: ParentTypesTyper =>
        new Descriptor() {
          def basicInfo = "Determine types of the parents"
          def fullInfo  = "Determine types the parents: %tree".dFormat(e.parents.map(snapshotAnyString).mkString)
        }
      
      case e: TypeInitialSuperTpeTyper =>
        new Descriptor() {
          def basicInfo = "Type first parent\n as supertype"
          def fullInfo  = {
            val t = treeAt(e.tree)
            "Preliminary super-type %tree" +
            "of type %tpe".dFormat(Some("Initial super type"), anyString(t), snapshotAnyString(t.tpe))
          }
        }
         
      case e: NewSuperTpePossiblyClass =>
        new Descriptor() {
          def basicInfo = "Replace non-class supertype"
          def fullInfo  = "Replace current non-class " +
                          "supertype " + snapshotAnyString(e.supertpt) + " with its first parent " +
                           snapshotAnyString(e.supertpt) + " (maybe a class). Also add old supertype to mixins."
        }
     
      case e: SuperTpeToPolyTpeTypeConstr =>
        new Descriptor() {
          def basicInfo = "Polymorphic supertype"
          def fullInfo  = "Polymorphic super type: " + snapshotAnyString(e.newSupertpt)
        }
      
      case e: ConvertConstrBody =>
        new Descriptor() {
          def basicInfo = "Convert constructor"
          def fullInfo  = "Convert constructor body\n" + snapshotAnyString(e.tree)
        }
        
      case e: ValidateParentClass =>
        new Descriptor() {
          def basicInfo = "Validate parent class"
          def fullInfo  = "Validate parent class %tree".dFormat(Some("Validate parent"), snapshotAnyString(e.parent))
        }
        
      case e: ValidateSelfTpeSubtypingTyper =>
        new Descriptor() {
          def basicInfo = "Self-type is a subtype of the parent type"
          def fullInfo  = "Self-type: %tpe" +
                          "is " + (if (e.subtypingRes) "" else "not") + "\n" +
                          "\nParent type %tpe".dFormat(Some("Self-type check"), snapshotAnyString(e.selfTpe), snapshotAnyString(e.parentTpe))
        }
       
      case e: ValDefTyper =>
        new Descriptor() {
          val tree = treeAt(e.valdef)
          def basicInfo = {
            val sym = SymbolSnapshot(tree.symbol)
            "Type value definition" + safeTypePrint(sym.tpe, "\n(typed as ", ")")
          }
          def fullInfo  = "Type value definition %tree".dFormat(Some("Type value definition"), anyString(tree))
        }
        
      case e: DefDefTyper =>
        new Descriptor() {
          def basicInfo = "Type definition"
          def fullInfo  = "Type definition %tree".dFormat(Some("Type Definition"), snapshotAnyString(e.tree))
        }
        
      case e: TypeDefTyper =>
        new Descriptor() {
          def basicInfo = "Type type definition"
          def fullInfo  = "Type type definiton %tree".dFormat(Some("Type type definition"), snapshotAnyString(e.tree))
        }

///--------------------------------------
///--------------------------------------

      case e: LabelDefTyper =>
        new Descriptor() {
          def basicInfo = "Type label"
          def fullInfo  = "Type label %tree".dFormat(Some("Type label"), snapshotAnyString(e.tree))
        }
            
      case e: DocDefTyper =>
        new Descriptor() {
          def basicInfo = "Type documentation definition"
          def fullInfo  = "Documentation " + snapshotAnyString(e.tree)
        }

      case e: AnnotatedTyper =>
        DEFAULT
        
      case e: BlockTyper =>
        new Descriptor() {
          def basicInfo = "Type block of statements"
          def fullInfo  = "%tree".dFormat(Some("Type block"), snapshotAnyString(e.tree))
        }
        
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
        new Descriptor() {
          def rep = if (e.constr) "Type constructor" else "Type function"
          def basicInfo = rep
          def fullInfo  = "%tree".dFormat(Some(rep), snapshotAnyString(e.tree))
        }

      case e: AssignTyper =>
        new Descriptor() {
          def basicInfo = "Type assignment"
          def fullInfo  = "Type assignment of \n" + snapshotAnyString(e.value1) +
                          "\nto " + snapshotAnyString(e.value2) 
        }
        
      case e: AssignLeftTyper =>
        new Descriptor() {
          def basicInfo = "Type left side of the assignment"
          def fullInfo  = "Type left side of the assignment of symbol " +
                          snapshotAnyString(e.treeSym)
        }

      case e: AssignGetterTyper =>
        new Descriptor() {
          def basicInfo = "Type getter for left side of the assignment"
          def fullInfo  = ""
        }
  
      case e: AssignRightTyper =>
        new Descriptor() {
          def basicInfo = "Type right side of the assignment"
          def fullInfo  = ""
        }
        
      case e: IfTyper =>
        new Descriptor() {
          def basicInfo = "Type if conditional"
          def fullInfo  = ""
        }
  
      case e:IfCondTyper =>
        new Descriptor() {
          def basicInfo = "Type condition"
          def fullInfo  = ""
        }

      case e:IfBranchTyper =>
        new Descriptor() {
          def basicInfo = "Type " + (if (e.cond) "then" else "else") + " branch"
          def fullInfo  = "With expected type: " + snapshotAnyString(e.pt)
        }
        
      case e:IfLubTyper =>
        new Descriptor() {
          def basicInfo = "Determine least-upper-bound for if conditional"
          def fullInfo  = snapshotAnyString(e.tree1) + ": " + snapshotAnyString(e.value1) +
                          " and " +
                          snapshotAnyString(e.tree2) + ": " + snapshotAnyString(e.value2) + "\n" +
                          "with expected type: " + snapshotAnyString(e.pt)

        }
        
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
        new Descriptor() {
          def basicInfo = "Type new instance"
          def fullInfo  = snapshotAnyString(e.tree)
        }

      case e:NewTypeCtorTyper =>
        DEFAULT
        
      case e:NewTypeCtorWithParamsTyper =>
        new Descriptor() {
          def basicInfo = "Type constructor with type parameters"
          def fullInfo  = "Type parameters: " + e.params.map(snapshotAnyString).mkString(",")
        }
         
      case e:EtaTyper =>
        DEFAULT
        
      case e:EtaByNameParamTyper =>
        DEFAULT
        
      case e:EtaEmptyPolyTyper =>
        DEFAULT
        
      case e:EtaPolyDoneTyper =>
        DEFAULT
  
      case e:EtaMethodTypeTyper =>
        new Descriptor() {
          def basicInfo = "Perform eta-expansion adaption for method" 
          def fullInfo  = {
            val t = treeAt(e.tree)
            "Eta-expand %tree" + 
            "with type %tpe" +
            "and parameters %sym".dFormat(Some("Eta-expand tree"), anyString(t), snapshotAnyString(t.tpe), e.params.map(snapshotAnyString).mkString(", "))
          }
        }
         
      case e:TryTypedArgsTyper =>
        new Descriptor() {
          def basicInfo = "Typecheck arguments individually first"
          def fullInfo  = "Typecheck arguments without taking into account expected type".dFormat(Some("Typecheck arguments"))
        }
        
      case e:TryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Try typechecking application \n of function to arguments"
          def fullInfo  = {
                          val t = treeAt(e.tree)
                          "Typecheck application of function to the arguments.\n" +
                          "If that fails adapt function to the arguments.\n" +
                          "Function: " + anyString(t) + "\n" +
                          "of type: " + snapshotAnyString(t.tpe) + "\n" +
                          "arguments: " + e.args.mkString("(", ",", ")") + "\n" + 
                          "with expected type " + snapshotAnyString(e.pt)
          }
        }

      case e:SuccessTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Typed application"
          def fullInfo  = ""
        }
        
      case e:SecondTryTypedApplyStartTyper =>
        new Descriptor() {
          def basicInfo = "Second attempt: \n " +
                          "Try adapting function to the given arguments"
          def fullInfo  = "Since first adapt at simply typing application of function\n" +
                          snapshotAnyString(e.fun) + "\n to arguments " +
                          e.args.map(snapshotAnyString).mkString("'", ",", "'") + "\n" +
                          "failed, try adapting function to the arguments"
        }

      case e:SecondTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Second attempt: \nType application of adapted function to arguments"
          def fullInfo  = "Second attempt at applying adapted function  '" + snapshotAnyString(e.qual1) +
                          "' to arguments '" + snapshotAnyString(e.args1) + "'"
        }          

      case e:FailedSecondTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Failed adapting function to the arguments"
          def fullInfo  = (if (e.args1 eq null) "Failed typing arguments '"
                          else "Failed adapting qualifier to arguments '") +
                          e.args1.map(snapshotAnyString).mkString("[", ",", "]") + "'"
        }

      case e:FailedTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Second attempt failed"
          def fullInfo  = ""
        }
        
      case e:TypedWildcardTyper =>
        DEFAULT
        
      case e:TypedEtaTyper =>
        new Descriptor() {
          def basicInfo = "Type expression and \n eta-expand it"
          def fullInfo  = {
            val expr  = treeAt(e.expr)
            val exprTpe = TypeSnapshot(expr.tpe)
            "Eta-expansion on expression " + anyString(expr) + 
              (if (exprTpe != null) "\n of type " + anyString(exprTpe) else "")
          }
        }
      
      case e: TypedTypedExprTyper =>
        new Descriptor() {
          def basicInfo = "Type explicitly typed expression"
          def fullInfo  = "Expression to be typed: " + snapshotAnyString(e.tree)
        }

      case e: TypeApplyTyper =>
        new Descriptor() {
          def basicInfo = "Type type application"
          def fullInfo  = "Type type application." + 
                          "\nApply types: " + e.args.map(snapshotAnyString) + 
                          "\n to type constructor " + snapshotAnyString(e.fun) + 
                          "\n with expected type: " + snapshotAnyString(e.pt)
        }
         
      case e:TypedTypeApplySuccessTyper =>
        new Descriptor() {
          def basicInfo = "Typechecked type application"
          def fullInfo  = "Typechecked type application: " + e.args.map(snapshotAnyString) +
                          " applied to " + snapshotAnyString(e.fun) +
                          " as " + snapshotAnyString(e.resultTpe)
        }
         
      case e:TypedApplyStableTyper =>
        DEFAULT
      
      case e:TypedApplyUnstableTyper =>
        DEFAULT
      
      case e:SuccessTypedApplyFunTyper =>
        new Descriptor() {
          def basicInfo = "Successfully typed function as\n " + snapshotAnyString(e.expectedFunPt)
          def fullInfo  = ("Function %tree was typed as %tpe" +
                          "in the context of the expected type %tpe").dFormat(
                            Some("Typed function"), snapshotAnyString(e.tree), snapshotAnyString(e.expectedFunPt), snapshotAnyString(e.pt))
        }
        
      case e:TypedApplyToAssignment =>
        DEFAULT
        
      case e:ApplyBlockTyper =>
        new Descriptor() {
          def basicInfo = "Type block application"
          def fullInfo  = ""
        }
        
      case e:ApplyTyper =>
        new Descriptor() {
          def basicInfo = "Type application"
          def fullInfo  = {
            val app1 = treeAt(e.app)
            "Type application of function \n" +
              snapshotAnyString(app1.fun) + "\n" +
              "to arguments " + app1.args.map(a => {val a0 = treeAt(a); anyString(a0) + ":" + snapshotAnyString(a0.tpe)}).mkString("(", ",", ")") +
              (e.e match {
                 case DefaultExplanation => ""
                 case _ => "\n" + e.toString + (if (settings.debugTD.value == "event") " expl: " + e.e.getClass else "")
              })
          }
        }

      case e:ApplyDynamicTyper =>
        DEFAULT
        
      case e:SuperTyper =>
        new Descriptor() {
          def basicInfo = "Type super"
          def fullInfo  = ""
        }
        
      case e:ThisTyper =>
        new Descriptor() {
          def basicInfo = "Type 'this'"
          def fullInfo  = ""
        }

      case e:SelectTyper =>
        new Descriptor() {
          def basicInfo = "Type member selection \n given correct qualifier"
          def fullInfo  = {
            val qual1 = treeAt(e.qual)
            "Type selection of \n" +
            snapshotAnyString(e.qual) + ".'" + e.name + "'\n" +
            "with qualifier of type \n" + snapshotAnyString(qual1.tpe) + "\n" +
            "and expected type " + snapshotAnyString(e.pt)
          }
        }
        
      case e:SelectTreeTyper =>
        new Descriptor() {
          def basicInfo = "Type member selection"
          def fullInfo  = "Type qualifier \n" + snapshotAnyString(e.tree.asInstanceOf[Select].qualifier) +
                          "\ncalling member '" + e.tree.asInstanceOf[Select].name + "'"
        }
  
      case e:SelectConstrTyper =>
        new Descriptor() {
          def basicInfo = "Type super-constructor call"
          def fullInfo  = ""
        }
       
      case e:TreeSymSelectTyper =>
        DEFAULT
  
      case e:SymSelectTyper =>
        new Descriptor() {
          def basicInfo = if (e.sym == NoSymbol) "No symbol found corresponding to\n the member qualifier"
                          else "Found symbol corresponding to the member \nof the qualifier"
          def fullInfo  = {
            val sym1 = SymbolSnapshot(e.sym)
            "Result of finding symbol corresponding to the member '" + snapshotAnyString(e.member) + "' of qualifier: \n" +
              snapshotAnyString(e.qual) + "\n" +
              (if (e.sym == NoSymbol) "Symbol not found "
              else (" Found symbol " + anyString(sym1) + "\n with type " + snapshotAnyString(sym1.tpe)))
          }
        }
       
      case e:SymExistsSelectTyper =>
        new Descriptor() {
          def basicInfo = "Valid symbol"
          def fullInfo  = "Valid symbol " + snapshotAnyString(e.sym)
        }

      case e:StabilizeTreeTyper =>
        DEFAULT
        
      case e:DeferredTypeTreeTyper =>
        DEFAULT

      case e:IdentTyper =>
        new Descriptor() {
          def basicInfo = "Type identifier: " + e.tree.asInstanceOf[Ident].name +
                          safeTypePrint(e.tree.symbol.tpe, "\n(typed as ", ")")
          def fullInfo  = ""
        }
        
      case e:LiteralTyper =>
        new Descriptor() {
          val tree1 = treeAt(e.tree)
          def basicInfo = "Type literal:\n" + anyString(tree1)
          def fullInfo  = "Typing " + e.tree.asInstanceOf[Literal].value +
                           (if (tree1.tpe != null) "of type " + snapshotAnyString(tree1.tpe) else "") +
                           "\nwith expected type " + snapshotAnyString(e.pt)
        }
         
      case e:SingletonTypeTreeTyper =>
        DEFAULT
        
      case e:SelectFromTypeTreeTyper =>
        DEFAULT
        
      case e:CompoundTypeTyper =>
        DEFAULT
        
      case e:AppliedTypeTyper =>
        new Descriptor() {
          def basicInfo = "Type applied type"
          def fullInfo  = "Type applied type \n" + snapshotAnyString(e.tree) + "\n" +
                          "with arguments " + e.args.mkString(",")
        }
  
      // TODO: there seems to be a bug related to range positions, hence 
      // def foo[T] will not show correctly the position for T
      case e:TypeBoundsTyper =>
        new Descriptor() {
          def basicInfo = "Type type-bounds"
          def fullInfo  = "Bounds: " + e.bounds
          }

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
          new Descriptor() {
            def basicInfo = e.info match {
                case ClassTemplate  => "Type class template"
                case ModuleTemplate => "Type object template"
                case TraitTemplate  => "Type trait template"
              }
            def fullInfo  = snapshotAnyString(e.templ) + " \n with parents: " +
                            e.parents.map(p =>
                              { val p0 = treeAt(p); anyString(p0) + ": " + p0.tpe}).mkString(",") +
                                " for " + snapshotAnyString(e.clazz)
          }

      case e:SelfTpeRefinedType =>
        DEFAULT
        
      // TODO: only for debugging purposes?
      case e:SelfTpeThis =>
        DEFAULT
        
      case e:AdaptToArgumentsTyper =>
        new Descriptor() {
          def basicInfo = "Adapt function to arguments"
          def fullInfo  = "Adapt qualifier " + snapshotAnyString(e.tree) + "\n" +
                          "so that it contains a function " + e.name + "\n" +
                          "that applies to arguments " + e.args.map(snapshotAnyString).mkString("(", ",", ")") + "\n" +
                          "with the expected type " + snapshotAnyString(e.pt)
        }

      case e:FinishedAdaptToArgumentsTyper =>
        new Descriptor() {
          def basicInfo = if (e.value1 eq e.value2) "Failed all attempts to adapt function to arguments"
                          else "Adapted function to the arguments"
          def fullInfo  = if (e.value1 eq e.value2) "Failed all attempts to adapt qualifier \n" + e.value1
                          else "Adapted qualifier \n" + e.value1 + "\n" + "with the transformation: \n" + e.value2
        }
        
      case e:FallbackAdaptToArgumentsTyper =>
        new Descriptor() {
          def basicInfo = "Fallback: adapt qualifier\n without any expected type"
          def fullInfo  = "Adapt qualifier to the member but without any expected return type"
        }
         
      case e:AdaptToMemberTyper =>
        new Descriptor() {
          def basicInfo = "Adapt to member"
          def fullInfo  = "Infer view which adapts current tree " + snapshotAnyString(e.tree) + "\n" +
                          "of type " + snapshotAnyString(e.tpe) + "\n" +
                          "to the template type (enforced by member) " + snapshotAnyString(e.searchTpe)
        }
         
      case e:OpenExistentialAdaptToMemberTyper => 
        DEFAULT
        
      case e:FoundCoercionAdapToMemberTyper =>
        new Descriptor() {
          def basicInfo = "Found coercion"
          def fullInfo  = "Found coercion \n" + snapshotAnyString(e.coercion) + "\n" +
                          "that adapts qualifier to the expected type"
        }
         
      case e:FailedAdaptToMemberTyper =>
        new Descriptor() {
          def basicInfo = "View inference for adaptation \n FAILED"
          def fullInfo  = "Failed inference of a view that adapts the tree \n" +
                          snapshotAnyString(e.tree) + "\n" +
                          "of type " + snapshotAnyString(e.tpe) + "\n" +
                          "to type " + snapshotAnyString(e.searchTpe)
        }
      
      case e:IsNotAdaptableTyper =>
        new Descriptor() {
          def basicInfo = "Qualifier is not adaptable \n FAILED"
          def fullInfo  = "Qualified " + snapshotAnyString(e.tree) + " with type " + 
                          snapshotAnyString(e.tpe) + " is not adaptable"
        }
         
      case e:InferViewAdaptToMemberTyper =>
        new Descriptor() {
          def basicInfo = "Infer view that adapts \n qualifier to member"
          def fullInfo  = "Infer view which adapts tree.\n" +
                          "Current type:  " + snapshotAnyString(e.value1) + "\n" +
                          "Expected type: " + snapshotAnyString(e.value2)
        }
         
      case e:DoTypedApplyTyper =>
        new Descriptor() {
          def argsToString(args0: List[Tree]) = args0.map(a => { val a0 = treeAt(a)
            anyString(a0) + ": " + (if (a0.tpe == null) "?" else snapshotAnyString(a0.tpe))
            }).mkString("(", ",", ")")
          def basicInfo = "Typecheck application of\n function to arguments"
          def fullInfo  = ("Typecheck application of function %tree" +
                           "to arguments %tree" +
                           "with expected type: %tpe").dFormat(Some("Application of function to arguments"),
                              snapshotAnyString(e.fun), argsToString(e.args), snapshotAnyString(e.pt))
        }
         
      case e:OverloadedSymDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Quick alternatives filter\n for overloaded function"
          def fullInfo  = "[Compiler optimization]\n Quickly filter-out alternatives for " +
                          snapshotAnyString(e.sym) + "\n" +
                          "and argument types " + e.argTypes.map(snapshotAnyString).mkString(",")
        }

      case e:CheckApplicabilityAlternativeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Verify alternative symbol"
          def fullInfo  = "Verifying applicability of the " + snapshotAnyString(e.alt) + " alternative " +
                          "with type " + snapshotAnyString(e.alt.tpe) + "\n" +
                          "for symbol " + snapshotAnyString(e.funSym) + " in the context of type " + snapshotAnyString(e.pt)
        }

      case e:IsApplicableAlternativeDoTypedApply =>
        new Descriptor() {
          def isApplicable = if (e.applicable) "applicable" else "not applicable"
          def basicInfo = "Alternative is " + isApplicable
          def fullInfo  = "Alternative for " + snapshotAnyString(e.sym) + " with type " +
                          snapshotAnyString(e.ftpe) + "\nis " + isApplicable
        }          
         
      case e:FilteredDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Filtered-out alternatives"
          def fullInfo  = {
            val funSym1 = SymbolSnapshot(e.funSym)
            "Initial filter out for symbol alernatives for tree " + snapshotAnyString(e.tree) +
            " results in symbol " + anyString(funSym1) + " of type " + snapshotAnyString(funSym1.tpe)
          }
        }
         
      case e:OverloadedTpeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Typecheck application \n for overloaded method"
          def fullInfo  = "Typechecking application of " + snapshotAnyString(e.fun) + " for overloaded method"
        }

      case e:InferMethodAlternativeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Infer correct method \n for application \n from alternatives"
          def fullInfo  = ""
        }
  
      case e:AdaptInferredMethodAlternativeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Adapt inferred method alternative"
          def fullInfo  = {
            val fun1 = treeAt(e.fun)
            "Adapt inferred " + anyString(fun1) + " of type " + snapshotAnyString(fun1.tpe)
          }
        }
      
      case e:InferredMethodDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Typecheck arguments application \nfor the inferred method"
          def fullInfo  = {
            val fun1 = treeAt(e.fun)
            "Do typed apply on an inferred and adapted method " + anyString(fun1) +
            " of type " + snapshotAnyString(fun1.tpe)
          }
        }
         
      case e:MethodTpeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Function with Method Type"
          def fullInfo  = "parameter symbols: '" + e.params.map(snapshotAnyString) +
                          "' and their types: '" +
                          e.params.map(p => combinedSnapshotAnyString(p)(_.tpe)) + "'"
        }

      case e:TryTupleApplyDoTypedApply =>
        DEFAULT
        
      case e:PackArgsDoTypedApply =>
        DEFAULT
        
      case e:PackedArgsDoTypedApply =>
        DEFAULT
        
      case e:TryNamesDefaultsDoTypedApply =>
        DEFAULT
        
      case e:CorrectArgumentsDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Number of arguments agrees \nwith the number of parameters"
          def fullInfo  = "Number of arguments agrees with formals in tree:" +
                          "\n" + snapshotAnyString(e.tree) +
                          "\nFormals: " + e.formals.map(snapshotAnyString).mkString("(", ",", ")") +
                          "\nArguments " + e.args.map(snapshotAnyString).mkString("(", ",", ")")
        }

      case e:TParamsResolvedDoTypedApply =>
        new Descriptor() {
          def basicInfo = "All type parameters are resolved"
          def fullInfo  = "All type parameters (if any) in typed application were resolved for tree:" +
                          snapshotAnyString(e.tree)
        }

      case e:ApplyTreeDoneDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Successfully typed application"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            "Type application in \n" + anyString(tree1) + "\n" +
            "as " + snapshotAnyString(tree1.tpe)
          }
        }
         
      case e:NeedsInstantiationDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Method Type needs instantiation"
          def fullInfo  = ""
        }
        
      case e:MethodTpeWithUndetTpeParamsDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Method Type with undetermined type parameters.\nNeed to resolve them first."
          def fullInfo  = ("Expression %tree" +
                           "of Method Type\n with\n" +
                           "- undetermined type parameters: %sym" + 
                           "- types of formal parameters %tpe").dFormat(Some("Method Type with unresolved type parameters"),
                              snapshotAnyString(e.tree), e.tparams.map(snapshotAnyString).mkString(","),
                              e.formals.map(snapshotAnyString).mkString("[", ",", "]"))
                          
        }

      case e:ProtoTypeArgsDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Infer prototype arguments"
          def fullInfo  = "Infer prototype arguments:\n" +
                          e.tparams.map(snapshotAnyString).mkString("[", ",", "]") +
                          " for " + snapshotAnyString(e.resultTpe)
        }
        
      case e:InstantiatedDoTypedApply =>
        new Descriptor() {
          val fun1 = treeAt(e.fun)
          def basicInfo = "Typecheck inferred instance" + safeTypePrint(fun1.tpe, "\n", "\n") + " in the application"
          def fullInfo  = "Typecheck inferred instance for \n" +
                          snapshotAnyString(e.fun) + "\nwith type " + snapshotAnyString(fun1.tpe) +
                          (if (!e.undet.isEmpty) "\n and still undetermined type parameters " +
                            e.undet.map(snapshotAnyString).mkString(",") else "") +
                            "\n and expected type " + snapshotAnyString(e.pt)
        }
         
      case e:DoTypedApplyDone =>
        new Descriptor() {
          def basicInfo = if (e.tree.isErrorTyped) "Failed to type application"
                          else "Typechecked application"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            "Applied arguments in the tree \n " + snapshotAnyString(tree1) + "\n" + 
            "of type " + snapshotAnyString(tree1.tpe)
          } 
        }

      case e:SingleTpeDoTypedApply =>
        DEFAULT
        
      case e:UnapplyDoTypedApply =>
        DEFAULT
        
      case _ =>
        new Descriptor() {
          def basicInfo = ev.lName
          def fullInfo  = ev.toString
        }
    }
  }
}