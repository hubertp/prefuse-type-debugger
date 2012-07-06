package scala.typedebugger
package stringops

trait NamerStringOps {
  self: StringOps with internal.CompilerInfo with internal.PrefuseStructure =>
    
  import global._
  import EV._
  import util.StringFormatter._
    
  trait NamerEventsOps {
    self: Descriptors =>
    private val DEFAULT = new DefaultDescriptor("namer")
    
    def explainNamerEvent(ev: Event with NamerEvent, uiNode: UINode[PrefuseEventNode])(implicit time: Clock = ev.time) = ev match {
      case e: NamerDone =>
        DEFAULT
        
      case e: TypeSigNamer => // todo: not used?
        new Descriptor {
          def basicInfo = "Can we verify\n type's signature?"
          def fullInfo  = "Tree to be typed: %tree".dFormat(Some("Signature of a type"), snapshotAnyString(e.tree))
        }
       
      case e: ClassSigNamer =>
        new Descriptor {
          def basicInfo = "Can we verify \n class' signature?"
          def fullInfo  = ("Completing class template %tree" +
          		             "having type params: %tree").dFormat(Some("Signature of a class"),
          		                 snapshotAnyString(e.templ), e.tparams.map(snapshotAnyString).mkString("[", ",", "]"))
        }
       
      case e: MethodSigNamer =>
        new Descriptor {
          def basicInfo = "Can we verify \n method's signature?"
          def fullInfo  = ("Completing method of type %tpe, having type parameters %tpe and RHS %tree").dFormat(
              Some("Signature of a method"), snapshotAnyString(e.tpt), e.tparams.map(snapshotAnyString).mkString("["," ,", "]"),
              snapshotAnyString(e.rhs))
        }
       
      case e: MissingParameterType =>
        new Descriptor {
          def basicInfo = "Missing parameter type"
          def fullInfo  = "Missing parameter type for %tree".dFormat(snapshotAnyString(e.tree))
        }
       
      case e: TypeDefSigNamer =>
        // TODO: distinguish type parameter from type member description
        new Descriptor {
          def basicInfo = "Can we verify \ntype's signature?"
          def fullInfo  = ("Completing type parameter/member definition %sym " +
          		            "with type parameters %tpe and RHS %tree").dFormat(Some("Signature of a type parameter/member"),
                          snapshotAnyString(e.tpsym), e.tparams.map(snapshotAnyString).mkString("[", ",", "]"),
                          snapshotAnyString(e.rhs))
        }
       
      case e: ModuleSigNamer =>
        new Descriptor {
          def basicInfo = "Can we verify \nobject's signature?"
          def fullInfo  = "Completing object %tree".dFormat(Some("Signature of an object"), snapshotAnyString(e.templ))
        }
       
      case e: ValDefSigNamer =>
        @inline
        def defaultKind = "value"

        val valueKind = uiNode.parent match {
          case Some(parent) =>
            parent.ev match {
              case MethodSigNamer(_, _, _, _) =>
                "parameter"
              case _                          =>
                defaultKind
            }
          case _            =>
            defaultKind
        }
        new Descriptor {
          def basicInfo = "Can we verify \n" + valueKind + "'s signature?"
          def fullInfo  = {
            val vdef1 = treeAt(e.vdef).asInstanceOf[ValDef]
            ("Completing the type of a value %sym. " +
            (if (vdef1.tpt.isEmpty) "Compute type from the body of the value " + anyString(vdef1.rhs)
             else "Is type " + anyString(vdef1.tpt) + " correct?")).dFormat(Some("Signature of a value"), vdef1.name.toString)
          }
        }

      case _ => DEFAULT

    }
  }
}