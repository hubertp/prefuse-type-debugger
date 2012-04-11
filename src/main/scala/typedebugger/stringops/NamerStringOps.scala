package scala.typedebugger
package stringops

trait NamerStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
    
  trait NamerEventsOps {
    self: Descriptors =>
    private val DEFAULT = new DefaultDescriptor("namer")
    
    def explainNamerEvent(ev: Event with NamerEvent)(implicit time: Clock = ev.time) = ev match {
      case e: NamerDone =>
        DEFAULT
        
      case e: TypeSigNamer =>
        new Descriptor {
          def basicInfo = "Typing of\n type's signature"
          def fullInfo  = "Tree to be typed: " + snapshotAnyString(e.tree)
        }
       
      case e: ClassSigNamer =>
        new Descriptor {
          def basicInfo = "Typing of\n class' signature"
          def fullInfo  = "Completing class " + snapshotAnyString(e.templ) +
            " with type params: " + e.tparams.map(snapshotAnyString)
        }
       
      case e: MethodSigNamer =>
        new Descriptor {
          def basicInfo = "Typing of\n method's signature"
          def fullInfo  = "Completing method of type " + snapshotAnyString(e.tpt) +
            " with type params: " + e.tparams.map(snapshotAnyString) +
            "\n" + snapshotAnyString(e.rhs)
        }
       
      case e: MissingParameterType =>
        new Descriptor {
          def basicInfo = "Missing parameter type"
          def fullInfo  = "Missing parameter type for " + snapshotAnyString(e.tree)
        }
       
      case e: TypeDefSigNamer =>
        // TODO: distinguish type parameter from type member description
        new Descriptor {
          def basicInfo = "Typing of\n type's signature"
          def fullInfo  = "Completing type parameter/member definition " + snapshotAnyString(e.tpsym) + " with type params: " + e.tparams.map(snapshotAnyString) +
            "\n" + snapshotAnyString(e.rhs)
        }
       
      case e: ModuleSigNamer =>
        new Descriptor {
          def basicInfo = "Typing \n object's signature"
          def fullInfo  = "Completing object " + snapshotAnyString(e.templ)
        }
       
      case e: ValDefSigNamer =>
        new Descriptor {
          def basicInfo = "Typing of\n value's signature"
          def fullInfo  = {
            val vdef1 = treeAt(e.vdef).asInstanceOf[ValDef]
            "Completing value's type " + vdef1.name +
            (if (vdef1.tpt.isEmpty) "" else ": " + snapshotAnyString(vdef1.tpt)) + "\n" +
            (if (vdef1.tpt.isEmpty) "Compute type from the body of the value " + anyString(vdef1.rhs) else "Type type " + anyString(vdef1.tpt))
          }
        }

      case _ => DEFAULT

    }
  }
}