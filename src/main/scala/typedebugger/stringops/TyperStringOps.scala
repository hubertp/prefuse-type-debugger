package scala.typedebugger
package stringops


trait TyperStringOps {
  self: StringOps with internal.CompilerInfo with util.DebuggerUtils =>
    
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
            val tpe0 = TypeSnapshot.mapOver(tree0.tpe)
            val sym0 = if (tree0.symbol != null) SymbolSnapshot.mapOver(tree0.symbol) else null
            val tpe = if (tpe0 != null) tpe0 else if (sym0 != null) TypeSnapshot.mapOver(sym0.tpe) else null
//            val tpeRes = if (e.tree.tpe != null) e.tree.tpe else (if (e.tree.symbol != null) e.tree.symbol.tpe else null)
            debug(if (tree0 != null) "Tree class " + tree0.getClass + " with sym " + sym0 else "", "event")
            //val resTpe = TypeSnapshot.mapOver(e.resTree._1.tpe)(e.resTree._2) // this will probably require further refinement
            val resTree = treeAt(e.resTree._1)(e.resTree._2)
            val resTpe  = TypeSnapshot.mapOver(resTree.tpe)(e.resTree._2)
            (e.expl + "\n\n" +
             "Typechecking tree: %ntree" +
             "Expected type: %tpe\n" +
             "Type of tree at the entry point: %tpe\n" +
             "Result type of the tree: %tpe").dFormat(Some("Typecheck tree"),
             anyString(tree0), snapshotAnyString(e.pt), anyString(tpe), anyString(resTpe))
          }
        }

      case e: TyperTyped1 =>
        new Descriptor() {
          def basicInfo = "Type tree"
          def fullInfo  = {
            ("Type tree %ntree" +
            "With expected type: %tpe").dFormat(Some("Type Tree"), snapshotAnyString(e.tree), snapshotAnyString(e.pt))
            //"Type tree " + snapshotAnyString(e.tree) +
            //"\nwith expected type: " + snapshotAnyString(e.pt)
          }
        }
        
      case e: TyperTypedDone =>
        new Descriptor() {
          def basicInfo = "Typechecked tree"
          def fullInfo  = {
            val t = treeAt(e.tree)
            val resT = e.tree.tpe
            ("Typeechecked tree %ntree" +
            "Tree has type %tpe").dFormat(Some("Typechecked Tree"), anyString(t), anyString(resT))
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
          def basicInfo = "Typechecking delayed"
          def fullInfo  = ("Statement %tree " +
                          "was not typechecked as targeted debugging was used.\n" +
                          "Statement was not on a direct path to the target selection").dFormat(Some("Omitted typechecking"),
                            anyString(e.tree))
        }
        
      case e: PackageTyper =>
        new Descriptor() {
          def withName  = if (e.tree.name == nme.EMPTY_PACKAGE_NAME) "default" else simpleName(e.tree.name)
          def basicInfo = "Can we type\n" + withName + " package?"
          def fullInfo  = "Type package %tree".dFormat(e.tree.asInstanceOf[RefTree].name.toString)
        }
        
      case e: ClassTyper =>
        new Descriptor() {
          def basicInfo = "Can we type\n" + simpleName(e.tree.name) + " class?"
          def fullInfo  = "Type class %tree".dFormat(snapshotAnyString(e.tree))
        }
      
      case e: ModuleTyper =>
        new Descriptor() {
          def basicInfo = "Can we type\n " + simpleName(e.tree.name) + " object?"
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
          def basicInfo = "Can we determine types of parents?"
          def fullInfo  = "Determine types of parents: %tree".dFormat(e.parents.map(snapshotAnyString).mkString)
        }
      
      case e: TypeInitialSuperTpeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type first parent\n and as set it as a main super type?"
          def fullInfo  = {
            val t = treeAt(e.tree)
            ("Preliminary super-type %tree " +
            "of type %tpe").dFormat(Some("Initial super type"), anyString(t), snapshotAnyString(t.tpe))
          }
        }
         
      case e: NewSuperTpePossiblyClass =>
        new Descriptor() {
          def basicInfo = "Current super type is a trait.\nReplacing super type with its first parent type"
          def fullInfo  = ("Replace current non-class of type %tpe with %tpe. " +
          		             "Adding previous supertype to the mixins list.").dFormat(Some("Replace super type"),
                           snapshotAnyString(e.supertpt0), snapshotAnyString(e.supertpt))
        }
      case e: SuperTpeParent           =>
        new Descriptor() {
          def basicInfo = "Super type has been established"
          def fullInfo  = "Set the super type to be %tree".dFormat(Some("Super type"), snapshotAnyString(e.supertpt))
        }
     
      case e: SuperTpeToPolyTpeTypeConstr =>
        new Descriptor() {
          def basicInfo = "Polymorphic super type"
          def fullInfo  = "Due to the existence of type parameters, super type is a PolyType: %tpe".dFormat(Some("Super type update"), snapshotAnyString(e.newSupertpt))
        }
      
      case e: ConvertConstrBody =>
        new Descriptor() {
          def basicInfo = "Convert constructor"
          def fullInfo  = "Convert constructor body %tree".dFormat(Some("Transforming constructor"), snapshotAnyString(e.tree))
        }
        
      case e: ValidateParentClass =>
        new Descriptor() {
          def basicInfo = "Can we validate parent class?"
          def fullInfo  = "Validate parent class %tree".dFormat(Some("Validate parent"), snapshotAnyString(e.parent))
        }
        
      case e: ValidateSelfTpeSubtypingTyper =>
        new Descriptor() {
          def basicInfo = "Is self-type a subtype of the parent type?"
          def fullInfo  = ("Self-type: %tpe " +
                          "is " + (if (e.subtypingRes) "" else "not") + "\n" +
                          "\nParent type %tpe").dFormat(Some("Self-type check"), snapshotAnyString(e.selfTpe), snapshotAnyString(e.parentTpe))
        }
       
      case e: ValDefTyper =>
        new Descriptor() {
          val tree = treeAt(e.valdef)
          def basicInfo = {
            val sym = SymbolSnapshot.mapOver(tree.symbol)
            "Can we type " + simpleName(e.tree.name) + " value definition?" + safeTypePrint(sym.tpe, "\n(typed as ", ")")
          }
          def fullInfo  = "Type value definition %tree".dFormat(Some("Type value definition"), anyString(tree))
        }
        
      case e: DefDefTyper =>
        new Descriptor() {
          def withName = {
            val DefDef(_, name, _, _, _, _) = e.tree
            visibleName(name) match {
              case Some(str) => " of " + str
              case None      => ""
            }
          }
          def basicInfo = "Can we type " + simpleName(e.tree.name) + " method definition?"
          def fullInfo  = "Type method definition %tree".dFormat(Some("Type method definition"), snapshotAnyString(e.tree))
        }
        
      case e: TypeDefTyper =>
        new Descriptor() {
          def basicInfo = "Can we type " + simpleName(e.tree.name) + " type definition?"
          def fullInfo  = "Type type definiton %tree".dFormat(Some("Type type definition"), snapshotAnyString(e.tree))
        }

///--------------------------------------
///--------------------------------------

      case e: LabelDefTyper =>
        new Descriptor() {
          def basicInfo = "Can we type " + e.tree.name + " label?"
          def fullInfo  = "Type label %tree".dFormat(Some("Type label"), snapshotAnyString(e.tree))
        }
            
      case e: DocDefTyper =>
        new Descriptor() {
          def basicInfo = "Can we type documentation definition?"
          def fullInfo  = "Documentation %tree".dFormat(Some("Type documentation"), snapshotAnyString(e.tree))
        }

      case e: AnnotatedTyper =>
        DEFAULT
        
      case e: BlockTyper =>
        new Descriptor() {
          def basicInfo = "Can we type block of statements?"
          def fullInfo  = "%tree".dFormat(Some("Type block"), snapshotAnyString(e.tree))
        }
        
      case e: AlternativeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type alternatives of patterns?"
          def fullInfo  = {
            val Alternative(alts) = treeAt(e.tree)
            "Type alternatives: %tree".dFormat(Some("Type alternatives of patterns"), alts.map(anyString).mkString(" | "))
          }
        }
        
      case e: StarTyper =>
        new Descriptor() {
          def basicInfo = "Can we type repetition of pattern?"
          def fullInfo  = {
            val Star(elem) = treeAt(e.tree)
            "Type repetition of %tree".dFormat(Some("Type repetition of pattern"), anyString(elem))
          }
        }

      case e: BindTyper =>
        new Descriptor() {
          def basicInfo = "Can we type bind of a variable " + simpleName(e.tree.name) + " in pattern?"
          def fullInfo  = {
            val Bind(name, body) = treeAt(e.tree)
            "Type binding of %tree to variable %sym".dFormat(Some("Type binding in pattern"), anyString(body), name.toString)
          }
        }
        
      case e: UnApplyTyper =>
        new Descriptor() {
          def basicInfo = "Can we type unapply in pattern?"
          def fullInfo  = ""
        }
        
      case e: ArrayValueTyper =>
        new Descriptor() {
          def basicInfo = "Can we type array of expressions?"
          def fullInfo  = ""
        }
        
      case e: FunctionTyper =>
        new Descriptor() {
          def rep = if (e.constr) "Can we type constructor?" else "Can we type function?"
          def basicInfo = rep
          def fullInfo  = "%tree".dFormat(Some(rep), snapshotAnyString(e.tree))
        }

      case e: AssignTyper =>
        new Descriptor() {
          def basicInfo = "Can we type assignment?"
          def fullInfo  = "Type assignment of %tree " +
                          "to %tree".dFormat(Some("Type assignment"), snapshotAnyString(e.value1), snapshotAnyString(e.value2)) 
        }

      // todo 
      case e: AssignGetterTyper =>
        new Descriptor() {
          def basicInfo = "Type getter for left side of the assignment"
          def fullInfo  = ""
        }
  
      // todo
      case e: AssignRightTyper =>
        new Descriptor() {
          def basicInfo = "Type right side of the assignment"
          def fullInfo  = ""
        }
        
      // replace by explanations?
      case e: IfTyper =>
        new Descriptor() {
          def basicInfo = "Can we type if/then/else conditional?"
          def fullInfo  = ""
        }
  
      case e:IfCondTyper =>
        new Descriptor() {
          def basicInfo = "Can we type condition?"
          def fullInfo  = ""
        }

      case e:IfBranchTyper =>
        new Descriptor() {
          def basicInfo = "Can we type " + (if (e.cond) "then" else "else") + " branch?"
          def fullInfo  = "Expected type %tpe".dFormat(Some("Type 'if' branch"), snapshotAnyString(e.pt))
        }
        
      case e:IfLubTyper =>
        new Descriptor() {
          def basicInfo = "What is the least upper bound of all conditional branches?"
          def fullInfo  = ("Least upper bound for branches:" +
          		            "%tree with type %tpe\n" +
          		            " and %tree with type %tpe\n" +
          		            "Expected type $tpe").dFormat(Some("Least upper bound for 'if' conditional"),
          		                snapshotAnyString(e.tree1), snapshotAnyString(e.value1),
          		                snapshotAnyString(e.tree2), snapshotAnyString(e.value2),
          		                snapshotAnyString(e.pt))
        }
        
      case e:MatchTyper =>
        new Descriptor() {
          def basicInfo = "Can we type match expression?"
          def fullInfo  = ""
        }
        
      // Start typed Return
      case e:ReturnTyper =>
        new Descriptor() {
          def basicInfo = "Can we type explicit return expression?"
          def fullInfo  = ""
        }
        
      case e:TryTyper =>
        new Descriptor() {
          def basicInfo = "Can we type 'try' construct?"
          def fullInfo = ""
        }
        
      case e:ThrowTyper =>
        new Descriptor() {
          def basicInfo = "Can we type 'throw' expression?"
          def fullInfo = ""
        }
        
      case e:NewTyper =>
        new Descriptor() {
          def basicInfo = "Is the new instance of " + snapshotAnyString(e.tree) + " type correct?"
          def fullInfo  = "Type the new instance of %tree".dFormat(Some("Type 'new'"), snapshotAnyString(e.tree))
        }

      case e:NewTypeCtorTyper =>
        DEFAULT
        
      case e:NewTypeCtorWithParamsTyper =>
        new Descriptor() {
          def basicInfo = "Constructor has some undetermined type parameters"
          def fullInfo  = "Type parameters %tpe".dFormat(Some("Type constructor with type parameters"), e.params.map(snapshotAnyString).mkString(","))
        }
        
      // todo: remove
      case e:EtaTyper =>
        DEFAULT
        
      case e:EtaByNameParamTyper =>
        DEFAULT
        
      case e:EtaEmptyPolyTyper =>
        DEFAULT
        
      case e:EtaPolyDoneTyper =>
        DEFAULT
  
      // todo:
      case e:EtaMethodTypeTyper =>
        new Descriptor() {
          def basicInfo = "Perform eta-expansion adaption for method" 
          def fullInfo  = {
            val t = treeAt(e.tree)
            "Eta-expand %tree " + 
            "with type %tpe " +
            "and parameters %sym".dFormat(Some("Eta-expand tree"), anyString(t), snapshotAnyString(t.tpe), e.params.map(snapshotAnyString).mkString(", "))
          }
        }
         
      case e:TryTypedArgsTyper =>
        new Descriptor() {
          def basicInfo = "Retry application of function to arguments.\n" +
          		            "Can we first typecheck arguments individually?"
          def fullInfo  = "Typecheck arguments without taking into account expected type".dFormat(
              Some("Typecheck arguments"))
        }
        
      case e:TryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Try typechecking application \n of function to arguments"
          def fullInfo  = {
                          val t = treeAt(e.tree)
                          ("Typecheck application of function to the arguments.\n" +
                          "If that fails adapt function to the arguments.\n" +
                          "Function: %tree of type: %tpe\n" + 
                          "arguments: %ntree" +  
                          "Expected type %tpe").dFormat(Some("Typechecking application"), anyString(t), snapshotAnyString(t.tpe), e.args.mkString("(", ",", ")"), snapshotAnyString(e.pt))
          }
        }

      case e:SuccessTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Function application has been typed"
          def fullInfo  = "Successfully typed application %tree with %tpe".dFormat(Some("Typed application"), snapshotAnyString(e.tree), snapshotAnyString(e.tpe))
        }
        
        
      // todo: remove
      case e:SecondTryTypedApplyStartTyper =>
        new Descriptor() {
          def basicInfo = "Fallback: \n " +
                          "Try adapting function to the given arguments"
          def fullInfo  = "Typing application %tree to argumentns %tree failed.\n" +
          		            "Try adapting function to arguments.".dFormat(
          		                Some("Adapt function to arguments"), snapshotAnyString(e.fun),
                              e.args.map(snapshotAnyString).mkString("'", ",", "'"))
        }

      case e:SecondTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Can we type application of adapted function\n to the original list of arguments?"
          def fullInfo  = "Type application of adapted function %tree to arguments %tree".dFormat(Some("Type application of adapted function"), 
                          snapshotAnyString(e.qual1), snapshotAnyString(e.args1))
        }          

      case e:FailedSecondTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Failed adapting function to the arguments"
          def fullInfo  = ((if (e.args1 eq null) "Failed typing arguments "
                          else "Failed adapting function to the arguments ") + "%tree").dFormat(
                            Some("Failed adaptation"), e.args1.map(snapshotAnyString).mkString("[", ",", "]"))
        }

      // todo: adv
      case e:FailedTryTypedApplyTyper =>
        new Descriptor() {
          def basicInfo = "Error exists in the existing expression.\nNo second attempt will be made."
          def fullInfo  = ""
        }
        
      case e:TypedWildcardTyper =>
        DEFAULT
        
      case e:TypedEtaTyper =>
        new Descriptor() {
          def basicInfo = "Is the expression type correct\n and can it be eta-expanded?"
          def fullInfo  = {
            val expr  = treeAt(e.expr)
            val exprTpe = TypeSnapshot.mapOver(expr.tpe)
            "Eta-expansion on expression %tree %tpe".dFormat(Some("Eta-expand"),anyString(expr), 
              (if (exprTpe != null) "\n of type " + anyString(exprTpe) else ""))
          }
        }
      
      case e: TypedTypedExprTyper =>
        new Descriptor() {
          def basicInfo = "Can we type explicitly type-annotated expression?"
          def fullInfo  = "Expression to be typed: %tree".dFormat(Some("Type expression with type ascription"),
            snapshotAnyString(e.tree))
        }

      case e: TypeApplyTyper =>
        new Descriptor() {
          def basicInfo = "Can we type application of type constructor to type arguments?"
          def fullInfo  = ("Apply type arguments: %tpe\n" + 
                          "to type constructor %ntree" +  
                          "with expected type: %tpe").dFormat(Some("Type type application"),
                          e.args.map(snapshotAnyString).mkString(","), snapshotAnyString(e.fun),
                          snapshotAnyString(e.pt))
        }
        
      case e: VerifyTypeApplicationTyper =>
        new Descriptor() {
          def basicInfo = "Given typechecked type constructor and arguments, can we type type application?"
          def fullInfo  = "Type type application given typechecked " +
                          "type constructor and type arguments"
        }
        
      case e: VerifyTypeApplicationTyperResult =>
        new Descriptor() {
          def basicInfo = snapshotAnyString(e.tpe) + " \n is a type of type application"
          def fullInfo  = "Type application was typechecked as %tpe".dFormat(Some("Typechecked type apply"), snapshotAnyString(e.tpe))
        }
         
      // todo: remove
      case e:TypedTypeApplySuccessTyper =>
        new Descriptor() {
          def basicInfo = "Typechecked type application"
          def fullInfo  = "Typechecked type application: " + e.args.map(snapshotAnyString) +
                          " applied to " + snapshotAnyString(e.fun) +
                          " as " + snapshotAnyString(e.resultTpe)
        }
         
      // remove in favour explanations?
      case e:TypedApplyStableTyper =>
        new Descriptor() {
          def basicInfo = "Can we type application involving stable function?"
          def fullInfo  = ""
        }
      
      // todo: remove
      case e:TypedApplyUnstableTyper =>
        DEFAULT
      
      case e:SuccessTypedApplyFunTyper =>
        new Descriptor() {
          def basicInfo = "Function in application has been typed" + safeTypePrint(e.expectedFunPt, pre=" as :\n")//snapshotAnyString(e.expectedFunPt)
          def fullInfo  = {
            ("Function %tree was typed as %tpe " +
             "in the context of the expected type %tpe").dFormat(
             Some("Typed function"), snapshotAnyString(e.tree), snapshotAnyString(e.expectedFunPt), snapshotAnyString(e.pt))
          }
        }
        
      case e:TypedApplyToAssignment =>
        new Descriptor() {
          def basicInfo = "Can we type assignment operation?"
          def fullInfo  = {
            "Type assignment operation given the correct qualifier %tree".dFormat(
             Some("Type assignment"), snapshotAnyString(e.qual))
          }
        }
        
      // todo: adv
      case e:ApplyBlockTyper =>
        new Descriptor() {
          def basicInfo = "Can we type application of statements to its arguments?"
          def fullInfo  = ""
        }
        
      case e:ApplyTyper =>
        new Descriptor() {
          def basicInfo = "Can we type application of\n function to arguments?"
          def fullInfo  = {
            val app1 = treeAt(e.app)
            val additionalInfo = (e.e match {
                 case DefaultExplanation => ""
                 case _ => "\nIn " + e.toString + (if (settings.debugTD.value == "event") " expl: " + e.e.getClass else "")
              })
            ("Can we type application of function %tree to arguments %sym?" + additionalInfo).dFormat(Some("Type application"),
              snapshotAnyString(app1.fun), app1.args.map(a => {val a0 = treeAt(a); anyString(a0) + ":" + snapshotAnyString(a0.tpe)}).mkString("(", ",", ")"))
              
          }
        }

      case e:ApplyDynamicTyper =>
        DEFAULT
        
      case e:SuperTyper =>
        new Descriptor() {
          def basicInfo = "Can we type 'super'?"
          def fullInfo  = ""
        }
        
      case e:ThisTyper =>
        new Descriptor() {
          def basicInfo = "Can we type 'this'?"
          def fullInfo  = ""
        }

      case e:SelectTyper =>
        new Descriptor() {
          def basicInfo = "Can we type member selection?"
          def fullInfo  = {
            val qual1 = treeAt(e.qual)
            ("Type selection of %tree.%sym with qualifier of type %tpe\n" +
             "and expected type %tpe").dFormat(Some("Type Selection given Qualifier"),
             anyString(qual1), e.name.toString, snapshotAnyString(qual1.tpe), snapshotAnyString(e.pt))
          }
        }
        
      case e:SelectTreeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type qualifier and its (potential) member?"
          def fullInfo  = {
            val t = treeAt(e.tree.asInstanceOf[Select])
            
            "Type member selection involving qualifier %tree and member %sym".dFormat(
            snapshotAnyString(t.qualifier), t.name.toString)
          }
        }
  
      case e:SelectConstrTyper =>
        new Descriptor() {
          def basicInfo = "Can we type super constructor call?"
          def fullInfo  = ""
        }
       
      case e:TreeSymSelectTyper =>
        DEFAULT
  
      case e:SymSelectTyper =>
        new Descriptor() {
          def basicInfo = if (e.sym == NoSymbol) "No symbol was found corresponding to\n the member qualifier"
                          else "Found symbol corresponding to the member \nof the qualifier"
          def fullInfo  = {
            val sym1 = SymbolSnapshot.mapOver(e.sym)
            val tpeStr = if (e.sym  == NoSymbol)
              "" else snapshotAnyString(sym1.tpe)
            ("Symbol corresponding to the member %sym of the qualifier %tree " +
             (if (e.sym == NoSymbol) "was NOT found %tpe"
              else (" was found with type %tpe"))).dFormat(Some("Search for the member symbol"),
             snapshotAnyString(e.member), snapshotAnyString(e.qual), tpeStr)
          }
        }
       
      // todo: remove
      case e:SymExistsSelectTyper =>
        new Descriptor() {
          def basicInfo = "Found symbol corresponding to the qualifier"
          def fullInfo  = "Valid symbol " + snapshotAnyString(e.sym)
        }

      // todo: remove
      case e:StabilizeTreeTyper =>
        DEFAULT
        
      case e:DeferredTypeTreeTyper =>
        new Descriptor() {
          def basicInfo = "Defer bounds checking in type selection"
          def fullInfo  = ""
        }

      case e:IdentTyper =>
        new Descriptor() {
          def basicInfo = "Can we attribute identifier \n'" + e.tree.name +"'?" +
                          safeTypePrint(e.tree.symbol.tpe, "\n(typed as ", ")")
          def fullInfo  = "Can we find a symbol corresponding to the identifier %sym?".dFormat(Some("Identifier search"),
                          e.tree.asInstanceOf[Ident].name.toString)
        }
        
      case e:LiteralTyper =>
        new Descriptor() {
          val tree1 = treeAt(e.tree)
          def basicInfo = "Can we type literal\n " + anyString(tree1) + "?"
          def fullInfo  = "Typing %tree %tpe with expected type %tpe".dFormat(Some("Type literal"),
                           e.tree.asInstanceOf[Literal].value.toString,
                           (if (tree1.tpe != null) " of type " + snapshotAnyString(tree1.tpe) + " " else ""),
                            snapshotAnyString(e.pt))
        }
         
      case e:SingletonTypeTreeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type singleton type?"
          def fullInfo  = ""
        }
        
      case e:SelectFromTypeTreeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type type selection?"
          def fullInfo  = ""
        }
        
      case e:CompoundTypeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type compound type?"
          def fullInfo  = ""
        }
        
      case e:AppliedTypeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type type application?"
          def fullInfo  = "Typing applied type %tree to arguments %tree".dFormat(Some("Type type application"), 
                          snapshotAnyString(e.tree), e.args.mkString(","))
        }
  
      // TODO: there seems to be a bug related to range positions, hence 
      // def foo[T] will not show correctly the position for T
      case e:TypeBoundsTyper =>
        new Descriptor() {
          def basicInfo = "Can we type type bounds?"
          def fullInfo  = "Are type bounds %tree type correct?".dFormat(Some("Type type bounds"),
              snapshotAnyString(e.bounds)) 
        }

      case e:ExistentialTypeTyper =>
        new Descriptor() {
          def basicInfo = "Can we type existential type?"
          def fullInfo  = ""
        }

        // todo: hide
      case e:DeferredRefCheckTypeTyper =>
        DEFAULT
         
      // todo: hide, synthetic
      case e:TypeTreeTyper =>
        DEFAULT
         
      case e:ImportTyper =>
        new Descriptor() {
          def basicInfo = "Can we type import statement?"
          def fullInfo  = ""
        }

      // todo: hide
      case e:UnexpectedTyper =>
        DEFAULT
          
      case e:TemplateTyper =>
        new Descriptor() {
          def msg(kind: String) = "Can we type " + kind + " template?"
          def basicInfo = e.info match {
            case ClassTemplate  => msg("class")
            case ModuleTemplate => msg("object")
            case TraitTemplate  => msg("trait")
          }
          def fullInfo  = "Type %tree having parents %sym for %sym".dFormat(Some("Type template"),
                          snapshotAnyString(e.templ),
                          e.parents.map(p =>
                            { val p0 = treeAt(p); anyString(p0) + ": " + p0.tpe}).mkString(","),
                          snapshotAnyString(e.clazz))
        }

      // todo
      case e:SelfTpeRefinedType =>
        DEFAULT
        
      // TODO: only for debugging purposes?
      case e:SelfTpeThis =>
        DEFAULT
        
      case e:AdaptToArgumentsTyper =>
        new Descriptor() {
          def basicInfo = "Can we adapt qualifier to arguments?"
          def fullInfo  = ("Adapt qualifier %tree " +
                          "so that it contains a function %sym " +
                          "that applies to arguments %tree " +
                          "with the expected type %tpe").dFormat(Some("Adapt qualifier to arguments"),
                          snapshotAnyString(e.tree), e.name.toString, e.args.map(snapshotAnyString).mkString("(", ",", ")"), snapshotAnyString(e.pt))
        }

      case e:FinishedAdaptToArgumentsTyper =>
        new Descriptor() {
          def basicInfo = if (e.value1 eq e.value2) "Failed all attempts to adapt function to arguments"
                          else "Adapted function to the arguments"
          def fullInfo  = if (e.value1 eq e.value2) "Failed all attempts to adapt qualifier %tree".dFormat(snapshotAnyString(e.value1))
                          else "Adapted qualifier %tree with the transformation: %tree".dFormat(snapshotAnyString(e.value1), snapshotAnyString(e.value2))
        }
        
      case e:FallbackAdaptToArgumentsTyper =>
        new Descriptor() {
          def basicInfo = "Fallback: can we adapt qualifier\n to member without any expected type?"
          def fullInfo  = "Adapt qualifier to the member but without any expected type".dFormat(Some("Adapt without expected type"))
        }
         
      case e:AdaptToMemberTyper =>
        new Descriptor() {
          def basicInfo = "Can we adapt qualifier to member and its arguments?"
          def fullInfo  = ("Infer view which adapts current tree %tree " +
                          "of type %tpe to the template type (enforced by member) %tpe").dFormat(Some("Can we adapt qualifier?"),
                          snapshotAnyString(e.tree), snapshotAnyString(e.tpe), snapshotAnyString(e.searchTpe))
        }
         
      case e:OpenExistentialAdaptToMemberTyper => 
        DEFAULT
        
      case e:FoundCoercionAdapToMemberTyper =>
        new Descriptor() {
          def basicInfo = "Found coercion"
          def fullInfo  = ("Found coercion %tree that adapts qualifier to the expected type").dFormat(
                            Some("Found coercion"), snapshotAnyString(e.coercion))
        }
         
      case e:FailedAdaptToMemberTyper =>
        new Descriptor() { 
          def basicInfo = "Could not find a view necessary for adaptation"
          def fullInfo  = ("Failed inference of a view that would adapt tree %tree " +
                          "of type %tpe to type %tpe").dFormat(Some("Failed inference of a view"),
                          snapshotAnyString(e.tree), snapshotAnyString(e.tpe),
                          snapshotAnyString(e.searchTpe))
        }
      
      case e:IsNotAdaptableTyper =>
        new Descriptor() {
          def basicInfo = "Qualifier is not adaptable"
          def fullInfo  = "Qualified %tree of type %tpe is not adaptable".dFormat(
                          Some("Failed qualifier adaptation"),
                          snapshotAnyString(e.tree), snapshotAnyString(e.tpe))
        }
         
      case e:InferViewAdaptToMemberTyper =>
        new Descriptor() {
          def basicInfo = "Can we infer view that adapts \n qualifier to member?"
          def fullInfo  = ("Infer view which adapts expression tree.\n" +
                          "Current type:  %tpe\n" + 
                          "Expected type: %tpe").dFormat(Some("Infer view"),
                          snapshotAnyString(e.value1), snapshotAnyString(e.value2))
        }
         
      case e:DoTypedApplyTyper =>
        new Descriptor() {
          def argsToString(args0: List[Tree]) = args0.map(a => { val a0 = treeAt(a)
            anyString(a0) + ": " + (if (a0.tpe == null) "?" else snapshotAnyString(a0.tpe))
            }).mkString("(", ",", ")")
          def basicInfo = "Given typechecked function,\ncan we type its application to arguments?"
          def fullInfo  = ("Typecheck application of function %tree " +
                           "to arguments %tree " +
                           "with expected type: %tpe").dFormat(Some("Application of function to arguments"),
                              snapshotAnyString(e.fun), argsToString(e.args), snapshotAnyString(e.pt))
        }
         
      case e:OverloadedSymDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Overloaded method has multiple alternatives.\nCan we quickly filter out inapplicable ones\nbased on the expected type?"
          def fullInfo  = "Filter-out alternatives for %sym based on the expected type".dFormat(
                            Some("Alternatives filtering based on expected type"),
                            snapshotAnyString(e.sym))
        }

      case e:CheckApplicabilityAlternativeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Verify alternative"
          def fullInfo  = {
            "Verifying applicability of the %sym alternative of type %tpe for expected type %tpe".dFormat(
              Some("Is alternative applicable?"), snapshotAnyString(e.alt),
              snapshotAnyString(e.alt.tpe), snapshotAnyString(e.pt))
          }
        }

      case e:IsApplicableAlternativeDoTypedApply =>
        new Descriptor() {
          def isApplicable = if (e.applicable) "applicable" else "not applicable"
          def basicInfo = "Alternative is " + isApplicable
          def fullInfo  = "Alternative for %sym of type %tpe is %sym".dFormat(Some("Alternative applicability"),
                          snapshotAnyString(e.sym), snapshotAnyString(e.ftpe), isApplicable) 
        }          
         
      case e:FilteredDoTypedApply =>
        new Descriptor() {
          
          def msg(treeAfter: Tree) = {
            TypeSnapshot.mapOver(SymbolSnapshot.mapOver(treeAfter.symbol).tpe) match {
              case OverloadedType(_, _) => "Filtered out invalid alternatives"
              case _                    => "Single valid alternative remained"
                
            }
          }
          def basicInfo = {
            val tree1 = treeAt(e.tree)
            msg(tree1)
          }
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            msg(tree1)
          }
        }
      
      // todo: 
      case e:OverloadedTpeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Typecheck application \n for overloaded method"
          def fullInfo  = "Typechecking application of %tree for overloaded method".dFormat(snapshotAnyString(e.fun))
        }

      // todo
      case e:InferMethodAlternativeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Can we infer a single method alternative\n" +
          		            "for the overloaded method\n" +
          		            "that is suitable for the application?"
          def fullInfo  = ""
        }
  
      case e:AdaptInferredMethodAlternativeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Can we adapt the inferred method alternative?"
          def fullInfo  = {
            val fun1 = treeAt(e.fun)
            "Adapt inferred %tree of type %tpe".dFormat(Some("Adapt inferred method alternative"),
              anyString(fun1), snapshotAnyString(fun1.tpe))
          }
        }
      
      case e:InferredMethodDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Can we typecheck application\nof the inferred alternative to to arguments?"
          def fullInfo  = {
            val fun1 = treeAt(e.fun)
            "Typecheck application of the inferred and adapted method %tree of type %tpe".dFormat(
              Some("Typecheck application"), anyString(fun1), snapshotAnyString(fun1.tpe))
          }
        }
      
      // todo: 
      case e:MethodTpeDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Function with Method Type"
          def fullInfo  = "parameter symbols: '" + e.params.map(snapshotAnyString) +
                          "' and their types: '" +
                          e.params.map(p => combinedSnapshotAnyString(p)(_.tpe)) + "'"
        }

      case e:TryTupleApplyDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Can we type application of function\n to arguments packed in a tuple?"
          def fullInfo  = ""
        }
        
      case e:PackArgsDoTypedApply =>
        DEFAULT
        
      case e:PackedArgsDoTypedApply =>
        DEFAULT
        
      case e:TryNamesDefaultsDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Try application of named and default arguments"
          def fullInfo  = ""
        }
        
      // todo: 
      case e:CorrectArgumentsDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Number of arguments agrees \nwith the number of parameters"
          def fullInfo  = "Number of arguments agrees with formals in tree:" +
                          "\n" + snapshotAnyString(e.tree) +
                          "\nFormals: " + e.formals.map(snapshotAnyString).mkString("(", ",", ")") +
                          "\nArguments " + e.args.map(snapshotAnyString).mkString("(", ",", ")")
        }

      // todo: 
      case e:TParamsResolvedDoTypedApply =>
        new Descriptor() {
          def basicInfo = "All type parameters are resolved"
          def fullInfo  = "All type parameters (if any) in typed application were resolved for tree:" +
                          snapshotAnyString(e.tree)
        }

      // todo: 
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
          def basicInfo = "An expression of MethodType has some\nundetermined type parameters." +
          		            "Can we infer an expression instance?"
          def fullInfo  = ""
        }
        
      case e:MethodTpeWithUndetTpeParamsDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Function is of MethodType with\nundetermined type parameters.\n\n" +
          		"Can we resolve type parameters\n before typechecking the actual application?"
          def fullInfo  = ("Expression %tree " +
                           "of Method Type\n with\n" +
                           "- undetermined type parameters: %sym\n" + 
                           "- types of formal parameters %tpe").dFormat(Some("Method Type with unresolved type parameters"),
                              snapshotAnyString(e.tree), e.tparams.map(snapshotAnyString).mkString(","),
                              e.formals.map(snapshotAnyString).mkString("[", ",", "]"))
                          
        }

      case e:ProtoTypeArgsDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Can we infer lenient types for type arguments\nbased on the expected type and current type and constraints of the function?"
          def fullInfo  = ("Infer lenient type arguments: %tpe\n" +
                          "for %tpe").dFormat(Some("Lenient type arguments inference"),
                            e.tparams.map(snapshotAnyString).mkString("[", ",", "]"), snapshotAnyString(e.resultTpe))
        }
        
      case e:InstantiatedDoTypedApply =>
        new Descriptor() {
          val fun1 = treeAt(e.fun)
          def basicInfo = "Can we type application involving just inferred method instance " + safeTypePrint(fun1.tpe, "\n", "?")
          def fullInfo  = {
            val undets = (if (!e.undet.isEmpty) "\n and still undetermined type parameters " +
            e.undet.map(snapshotAnyString).mkString(",") else "")
            ("Typecheck inferred instance for %tree of type %tpe with expected type %tpe" + undets).dFormat(Some("Typecheck inferred instance"),
              anyString(e.fun), snapshotAnyString(fun1.tpe), snapshotAnyString(e.pt))
          }
        }
         
      case e:DoTypedApplyDone =>
        new Descriptor() {
          val t = treeAt(e.tree)
          def hasErrors = t.isErroneous || t.isErroneousApply
          
          def basicInfo = if (hasErrors) ("Typing application failed \n" +
          		"since the type is erroneous")
            else "Application is type correct"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            "Applied arguments in the tree %tree of type %tpe".dFormat(
            snapshotAnyString(tree1), snapshotAnyString(tree1.tpe))
          } 
        }

        
      // todo: remove from events
      case e:SingleTpeDoTypedApply =>
        DEFAULT
        
      case e:UnapplyDoTypedApply =>
        new Descriptor() {
          def basicInfo = "Try typechecking application of\n'unapply' to its arguments"
          def fullInfo  = ""
        }
        
      case _ =>
        new Descriptor() {
          def basicInfo = ev.lName
          def fullInfo  = ev.toString
        }
    }
  }
}