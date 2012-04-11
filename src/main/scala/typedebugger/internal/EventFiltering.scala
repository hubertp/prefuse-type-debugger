package scala.typedebugger
package internal

import scala.collection.mutable
import ui.Filtering

trait EventFiltering {
  self: internal.CompilerInfo =>
    
  import global.EV._
  import global.{ definitions, DefDef, nme }

  object FilteringOps {
    def map: PartialFunction[Event, Filtering.Value] =
      fromEventToEnum0
    
    private lazy val fromEventToEnum0 = new EventToEnumFunc()
    
    class EventToEnumFunc extends PartialFunction[Event, Filtering.Value] {
      // TODO: ideally size limited soft-references cache
      private[this] var cache = new mutable.WeakHashMap[Event, Option[Filtering.Value]]()
      
      def isDefinedAt(e: Event): Boolean = {
        cachedRes(e).isDefined
      }
      
      def apply(e: Event): Filtering.Value = {
        cachedRes(e) match {
          case Some(v) => v
          case None    => missingCase(e)
        }
      }
      
      private def cachedRes(e: Event): Option[Filtering.Value] = {
        if (!cache.contains(e)) {
          cache(e) = e match {
              case e: TyperTyped => handleTyperTyped(e)
              case _             => handleRest(e)
            }
        }
        cache(e)
      }
    }
    
    private def handleRest(e: Event): Option[Filtering.Value] = e match {
      case e: ProtoTypeArgsDoTypedApply =>
        Some(Filtering.ProtoTpeArgs)

      case e: CheckTypesCompatibility =>
        Some(Filtering.TypesComp)

      case e: SubTypeCheck =>
        Some(Filtering.SubCheck)

      case e: Subtyping =>
        Some(Filtering.Subtyping)

      case e: OverloadedSymDoTypedApply =>
        Some(Filtering.QuickAltFilter)

      case e: ImprovesAlternativesCheck =>
        Some(Filtering.AltComp)

      case e: ImplicitsEligibility =>
        Some(Filtering.ImplElig)

      case e: VerifyImplicit =>
        Some(Filtering.VerifyImpl)

      case _ =>
        None

    }
    
    private def handleTyperTyped(e: TyperTyped): Option[Filtering.Value] = {
      e.tree match {
        case ddef: DefDef if ddef.name == nme.CONSTRUCTOR =>
          Some(Filtering.Constr)
    
        case ddef: DefDef if (ddef.symbol != null && ddef.symbol.isSynthetic) =>
          Some(Filtering.DefSynth)
        case _ =>
          e.expl match {
            case e@TypeTemplateStatement(stat) if (stat.symbol != null) =>
              if (stat.symbol.isSynthetic || stat.symbol.isGetter) Some(Filtering.TemplateSynth) 
              else None
            case _ =>
              None
          }

      }
    }
  }
}