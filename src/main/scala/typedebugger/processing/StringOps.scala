package scala.typedebugger
package processing

import scala.tools.nsc.symtab

trait StringOps extends AnyRef
                with TyperStringOps
                with AdaptStringOps
                with InferStringOps
                with ImplicitsStringOps {
  self: internal.CompilerInfo =>
    
  import global.{Tree => STree, _}
  import EV._
  
  object Formatting {
    //val fmtFull = "[%ph] [%tg] %ev %po %id" // TODO this should be configurable
    //val fmtFull = "[%ph] [%tg] %ev" // TODO this should be configurable
    val fmtFull = "%ev"
    val fmt = "%ln"
  }
  
  object Explanations extends AnyRef
                      with TyperExplanations
                      with NamerExplanations
                      with AdaptExplanations {
    def apply(ev: TyperTyped): String = ev.expl match {
      case DefaultExplanation =>
        ev formattedString Formatting.fmt
      case tEV: TyperExplanation =>
        explainTyper(tEV)
      case nEV: NamerExplanation =>
        explainNamer(nEV)
      case aEV: AdaptExplanation =>
        explainAdapt(aEV)
      case iEV: InferExplanation =>
        "" //ev formattedString PrefuseEventNode.fmt
      case _ =>
        ev formattedString Formatting.fmt
    }
  }
  
  // TODO, provide full info as well
  object Events extends AnyRef
                with ErrorEvents
                with TyperEventsOps
                with AdaptEventsOps
                with InferEventsOps
                with ImplicitsEventsOps {
    def apply(e: Event, full: Boolean = false) = e match {
      case eEV: ContextTypeError =>
        explainError(eEV, full)
      case ev: TyperEvent        =>
        explainTyperEvent(ev)
      case ev: DoTypedApplyEvent =>
        explainTyperEvent(ev)
      case ev: AdaptToEvent      =>
        explainTyperEvent(ev)
      case ev: AdaptEvent        =>
        explainAdaptEvent(ev)
      case ev: InferEvent        =>
        explainInferEvent(ev)
      case ev: ImplicitEvent     =>
        explainImplicitsEvent(ev)
      case _                     =>
        (e formattedString Formatting.fmt, e formattedString Formatting.fmtFull)
    }
  }
  
  // TODO refactor
  trait TyperExplanations {
    def explainTyper(ev: Explanation with TyperExplanation): String = ev match {
      case _: TypePackageQualifier =>
        "Type package qualifier"
        
      case _: TypePackageStatement =>
        "Typecheck package member"
    
      case e@TypeTemplateStatement(stat) =>
         val parent = e.templateInfo match {
           case ClassTemplate => "class"
           case ModuleTemplate => "object"
           case TraitTemplate => "trait"
         }
         stat match {
           case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) =>
             "Typecheck " + parent + " constructor"
           case stat =>
             val mem = if (stat.symbol != NoSymbol && stat.symbol != null ) { ": '" + stat.symbol + "'" } else ""
             "Typecheck " + parent + " member" + mem
         }
        //"Typecheck member in template"

      case TypeExplicitTreeReturnType(_, tpe) =>
        //"Typecheck return type " + anyString(tpe) + " of the function"
        "Typecheck explicit return type"

      case _: TypeDefConstr => 
        "Typecheck body of the constructor"

      case _: TypeDefBody =>
        "Typecheck body of the definition"

      // Apply
      case _: TypeFunctionApply =>
        "Typecheck function"
        //"Typecheck function in the application"

      case _: TypeArgsApply =>
        "Typecheck arguments"
        //"Typecheck arguments in the application"
        //override def provide(a: Tree): Explanation = TypeArgApply(a)

      case _: TypeArgApply =>
        "Typecheck argument"
        //"Typecheck single argument for application"

      // Apply overloaded
      case _: TypeArgsInOverloadedApply => 
        "Typecheck arguments \n in overloaded application"
        // override def provide(a: Tree): Explanation = TypeArgInOverloadedApply(a)

      case _: TypeArgInOverloadedApply => 
        "Typecheck argument without \n expected type \n in overloaded application"

      //Eta
      case _: TypeEtaExpandedTreeWithWildcard => 
        "Typecheck eta-expanded tree\n without expected type involved"

      case _: TypeEtaExpandedTreeWithPt => 
        "Typecheck newly eta-expanded tree\n with expected type"

      //TypeApply
      case _: TypeFunctionTypeApply => 
        "Typecheck function\n in type application"

      case _: TypeHigherKindedTypeApplyWithExpectedKind =>
        "Typecheck higher-kinded type with expected kind"

      case _: TypeHigherKindedTypeApplyOverloaded =>
        "Typecheck higher-kinded overloaded polymorphic type"

      case _: TypeHigherKindedTypeForAppliedType =>
        "Typecheck higher-kinded type in applied type"

      //TypeFunction
      case _: TypeFunctionParameter =>
        "Typecheck function parameter"

      case _: TypeFunBody =>
        "Typecheck function body"

      //Do typed apply, try typed apply
      case _: TypeArgsStandalone =>
        "Typecheck arguments\n without expected type context"
        //"Typecheck arguments without taking into account formal parameter types, for " +
        //"further adaption of the qualifier"
        //override def provide(a: Tree): Explanation = TypeArgStandalone(a)

      //Do typed apply, try typed apply
      case _: TypeArgStandalone =>
        "Typecheck argument\n without expected type context"
        //"Typecheck argument without taking into account formal parameter type, for " +
        //"further adaption of the qualifier"

      //Block
      case _: TypeStatementInBlock =>
        "Typecheck statement"
        //"Normal typecheck for statemenet in block"
        //override def underlying = underlying0

      case _: TypeLastStatementInBlock =>
        "Typecheck last statement"
        //"Typecheck of the final statement in block"
        //override def underlying = underlying0

      //Select
      case _: TypeQualifierInSelect =>
        "Typecheck qualifier"
        //"Typecheck qualifier for select statemenent"

      case _: TypeSuperQualifier =>
        "Typecheck super-dot qualifier"
        //"Typecheck qualifier in super call"

      case _: TypeAdaptedQualifer =>
        "Typecheck qualifier adapted to member"
        //"Typecheck qualifier adapted to member"

      //Super
      case _: TypeQualifierInSuper =>
        "Typecheck qualifier in super-dot"

      //AdaptToMember
      case _: TypeAppliedImplicitView =>
        //"Typecheck qualifier with applied implicit view"
        "Typecheck application of inferred view\n that adapts qualifier"

      //ValDef
      case _: TypeValType =>
        "Typecheck value's type"

      //Type constructor
      case TypeTypeConstructor(tree: STree) =>
        "Typecheck type constructor " + anyString(tree)

      case _: TypeTypeConstructorInNew =>
        "Typecheck type constructor for 'new'" // TODO

      //Parent typing
      case _: TypeInitialSuperType =>
        "Typecheck first parent\n as initial supertype"
        //"Type first of the parents as the initial supertype"

      case TypeParentMixin(mixin) =>
        "Typecheck mixin\n" + anyString(mixin)
        //"Type of the mixed in parents"
 
      case _: TypeCurrentTraitSuperTpt =>
        "Typecheck current\n super-type"
        //"Typer current supertype that is trait\n" +
        //"to replace it with it's supertpe" 

      case _: TypeFirstConstructor =>
        "Typecheck transformed \n primary constructor"//TODO
        //"Type transformed first constructor"

      //Typed
      case _: TypeExplicitTypeAnnotation =>
        "Typecheck type annotation"
        //"Type explicit type annotation"  

      case _: TypeAnnotatedExpr =>
        "Typecheck type-annotated expression"
        //"Type type-annotated expression"

      // Bounds checking
      case TypeLowerBound(bound) =>
        bound match {
                  case Select(_, tpnme.Nothing) =>
                    "Typecheck lower bound:Nothing"
                  case _ =>
                    "Typecheck lower bound"
                }
        //"Type lower of the type bounds"

      case TypeHigherBound(bound) =>
        bound match {
                  case Select(_, tpnme.Any) =>
                    "Typecheck higher bound:Any"
                  case _ =>
                    "Typecheck higher bound"
                }
        //"Type higher of the type bounds"

      case _: TypeQualifierWithApply =>
                "Typecheck qualifier with \n 'apply()' member"

      //type parameters
      case _: TypeClassTypeParameter =>
        "Typecheck class type-parameter"

      case _: TypeHigherOrderTypeParameter =>
        "Typecheck higher-order type parameter"

      case _: TypeDefTypeParameter =>
        "Typecheck def \n type-parameter" 
        //"Type def type parameter"

      case TypeDefParameter(param) =>
        if (param.symbol.hasFlag(symtab.Flags.IMPLICIT)) "Typecheck implicit parameter" else "Typecheck parameter"
        //"Type def parameter"

      // refinement
      case _: TypeRefinementStatement =>
        "Typecheck refinement statement"

      case _: TypeExistentialTypeClause =>
        "Typecheck existential type-clause"

      // Use case?
      case _: TypeUseCaseStatement =>
        "Typecheck use-case statement"
        //"Type statement in the use case"
      
      case _ => "Typecheck ?"
    }
  }
  
  trait NamerExplanations {
    def explainNamer(ev: Explanation with NamerExplanation): String = ev match {
      case _: MethodReturnType =>
        "Typecheck return type of the method"
      
      case _: MethodReturnTypeAgain =>
        "Typecheck return type of the method (again) in Namer"

      case _: InferredMethodReturnType =>
        "Typecheck inferred return type of the method"

      case ValExplicitType(_, sym) =>
        "Typecheck type of a " + (if (sym.isMutable) "variable" else "value")

      case _: TypeAbstractTpeBounds =>
        "Typecheck bounds for the abstract type definition"

      case _: TypeValDefBody =>
        "Typecheck body of the value/variable\n" +
        "to infer its type"

      case _: TypeMethodDefBody =>
        "Typecheck body of the method"
        
      case _ =>
        "Typecheck ?"
    }
  }
  
  trait AdaptExplanations {
    def explainAdapt(ev: Explanation with AdaptExplanation): String = ev match {
      case _: TypeQualifierWithApply =>
        "Typecheck adapted qualifier with .apply()"
    
      case _: MethodEtaExpansion =>
        "Eta-expansion adaptation"
    
      case NotASubtypeInferView(treetp, pt) =>
        "Infer view to satisfy \n" + anyString(treetp) + " <: " + anyString(pt)

      case _: FirstTryTypeTreeWithAppliedImplicitArgs => 
        "Typecheck tree with applied \n (inferred) implicit arguments"
    
      case _: FallbackImplicitArgsTypeClean =>
        "Fallback\n Applying implicit arguments failed.\n " +
      	"Type and adapt original tree without expected type"
      case _: TypeTreeWithAppliedImplicitArgs => 
        "Typecheck tree with applied \n (inferred) implicit arguments"

      case _: TypeImplicitViewApplication =>
        "Typecheck application of found implicit view"
      
      case _ =>
        "Typecheck ?"

    }
  }
  
  // TODO, should be able to provide more details message
  trait ErrorEvents {
    def explainError(ev: Event with ContextTypeError, fullInfo: Boolean)= ev match {
      case ContextAmbiguousTypeErrorEvent(err, level) =>
        (level match {
          case ErrorLevel.Hard => "Ambiguous type error"
          case ErrorLevel.Soft => "(possibly) Recoverable ambiguous type error"
        }, "")
      
      case ContextTypeErrorEvent(err, level) =>
        (level match {
          case ErrorLevel.Hard => "Type error"
          case ErrorLevel.Soft => "(possibly) Recoverable type error"
        }, "Error:\n" + err.errMsg)
      
      case _ =>
        (ev formattedString Formatting.fmt, ev formattedString Formatting.fmtFull)
    }
  }
}