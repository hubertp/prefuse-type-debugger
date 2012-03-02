package scala.typedebugger
package processing

trait ImplicitsStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
    
  trait ImplicitsEventsOps {
    private val DEFAULT = ("(implicits| not-implemented)", "(implicits| not-implemented)")
    
    def explainImplicitsEvent(ev: Event with ImplicitEvent)(implicit time: Clock = ev.time) = ev match {
      case e: ImplicitDone =>
        val short = e.coercion match {
          case EmptyTree =>
            "No applicable implicit\n was found"
          case t if t.isEmpty =>
            "No applicable implicit\n was found"
          case _ =>
            "Found applicable implicit"
        }
      
        (short,
         "Found implicit: " + (if (e.coercion.isEmpty) "None" else e.coercion))
    
      case e: InferImplicit =>
        val descr = (if (e.byName) "By-name implicits" else "Implicits") + " search"
        val tree1 = treeAt(e.tree)
        val full = 
          "Implicits search for tree " + anyString(tree1) +
          "\nwith type " + snapshotAnyString(tree1.tpe) +
          "\nWith expected type " + snapshotAnyString(e.pt) +
          (if (!e.undetParams.isEmpty) "\n and undetermined type parameters " + e.undetParams.map(snapshotAnyString).mkString("[", ",", "]")
          else "")
       (descr, full)
       
      case e: ImprovesCompare =>
        ("Compare two implicits",
         "Comparing implicit:\n " + e.info1 + "\n vs. \n" + e.info2)
         
      case e: ImprovesResult =>
        (if (e.res) "Implicit improves" else "No improvement exists", "")

      case e: VerifyImplicit =>
        ("Verify available implicit",
         "Try implicit " + e.info.name + // TODO 
         "\nfor tree: " + snapshotAnyString(e.newTree) + "\nwith expected type " + snapshotAnyString(e.pt))

      case e: ImplicitSearchDone =>
        DEFAULT

      case e: SearchContextImplicits =>
        val implicits = e.allImplicits.map(_.map(info => snapshotAnyString(info.sym)).mkString("[", ",", "]")).mkString("\n")
        ("Search for applicable implicits\nin the scope",
         "Search appropriate implicits in the current scope\n" + 
         "For tree " + snapshotAnyString(e.tree) + "\n" +
         "All available implicits in the context:\n" + implicits)
         
      case e: ManifestAndExpectedTpeImplicits =>
        ("Implicit manifests or expected type",
         "Search appropriate manifest evidence or implicit for the expected type " + snapshotAnyString(e.pt))
         
      case e: SearchExpectedTypeImplicits =>
        ("Search for implicits based on\nexpected type",
         "Search implicits based on\n" +
         "expected type '" + snapshotAnyString(e.pt) + "'.\n" +
         "These are all implicits found in companion objects of classes C\n" +
         "such that some part of expected type has C as of its super-classes.")
         
      case e: SearchManifestImplicits =>
        ("Search evidence for manifest",
         "Search implicit for the manifest for type " + snapshotAnyString(e.tpe))
         
      case e: AllEligibleImplicits =>
        ("Filter all eligible implicits",
         "Filter all eligible implicits for expected type " + snapshotAnyString(e.pt))
      
      case e: AllEligibleImplicitsDone =>
        ("Filtered all eligible implicits","")
        
      case e: InfoEligibleTest =>
        ("Test implicit eligibility\n" + e.info.name, "Implicit: " + e.info.name)
        
      case e: InfoEligibleTestDone =>
        val short = if (e.eligible) "Implicit eligible" else "Implicit not eligible"
        (short, "")
         
      case e: CheckTypesCompatibility =>
        ("Check types compatibility " + (if (e.fast) "(quick)" else ""),
         "Check compatibility of types: \n" + snapshotAnyString(e.tp) + " vs. " + snapshotAnyString(e.pt))
        
      case e: CheckedTypesCompatibility =>
        val short  = if (e.res) "Types compatible" else "Types not compatible"
        (short, "")
        
      case e: AmbiguousImplicitsError =>
        ("Ambiguous implicits", 
         "Cannot apply implicit conversion due to ambiguity of two implicits:\n" +
         snapshotAnyString(e.info1Sym) + " of type " + snapshotAnyString(e.info1Tpe) + " and\n" + 
         snapshotAnyString(e.info2Sym) + " of type " + snapshotAnyString(e.info2Tpe))

      case e: PossiblyValidImplicit =>
        val prefix = if (e.result) "Valid" else "Invalid"
        val fullDescr =
          if (e.result)
            "Typechecked valid implicit converstion:\n" + snapshotAnyString(e.sym) + " of type " + snapshotAnyString(e.tpe)
          else 
            "Invalid implicit converstion:\n" + snapshotAnyString(e.sym) + " of type " + snapshotAnyString(e.tpe) 
        (prefix + " implicit", fullDescr)

      case e: CyclicReferenceInImplicitsImprove =>
        ("Cyclic reference while trying to improve implicit", "")
        
      case e: DivergentImplicitEvent =>
        ("Divergent implicit error", "")
      
      case _ => DEFAULT
    }
  }
}