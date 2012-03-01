package scala.typedebugger
package processing

trait NamerStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
    
  trait NamerEventsOps {
    private val DEFAULT = ("(namer| not-implemented)", "(namer| not-implemented)")
    
    def explainNamerEvent(ev: Event with NamerEvent)(implicit time: Clock = ev.time) = ev match {
      case e: NamerDone =>
        DEFAULT
        
      case e: TypeSigNamer =>
        ("Typing of\n type's signature", "Tree to be typed: " + snapshotAnyString(e.tree))
       
      case e: ClassSigNamer =>
        ("Typing of\n class' signature", "Completing class " + snapshotAnyString(e.templ) + " with type params: " + e.tparams.map(snapshotAnyString))
       
      case e: MethodSigNamer =>
        ("Typing of\n method's signature",
         "Completing method of type " + snapshotAnyString(e.tpt) + " with type params: " + e.tparams.map(snapshotAnyString) +
         "\n" + snapshotAnyString(e.rhs))
       
      case e: MissingParameterType =>
        ("Missing parameter type",  "Missing parameter type for " + snapshotAnyString(e.tree))
       
      case e: TypeDefSigNamer =>
        ("Typing of\n abstract type's signature",
         "Completing abstract type definition " + snapshotAnyString(e.tpsym) + " with type params: " + e.tparams.map(snapshotAnyString) +
        "\n" + snapshotAnyString(e.rhs))
       
      case e: ModuleSigNamer =>
        ("Typing \n object's signature", "Completing object " + snapshotAnyString(e.templ))
       
      case e: ValDefSigNamer =>
        ("Typing of\n value's signature",
         "Completing value's type " + e.name + ": " + snapshotAnyString(e.tpt) + "\n" +
        (if (e.tpt.isEmpty) "Compute type from the body of the value " else "Type type" )+
        "\n" + snapshotAnyString(e.rhs))
      case _ => DEFAULT
    }
  }
}