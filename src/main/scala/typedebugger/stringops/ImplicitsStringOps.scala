package scala.typedebugger
package stringops

trait ImplicitsStringOps {
  self: StringOps with internal.CompilerInfo =>
    
  import global._
  import EV._
  import util.StringFormatter._
    
  trait ImplicitsEventsOps {
    self: Descriptors =>
    private val DEFAULT = new DefaultDescriptor("implicits")
    
    def explainImplicitsEvent(ev: Event with ImplicitEvent)(implicit time: Clock = ev.time) = ev match {
      case e: ImplicitDone =>
        new Descriptor {
          def basicInfo = e.coercion match {
            case EmptyTree =>
              "No applicable implicit\n was found"
            case t if t.isEmpty =>
              "No applicable implicit\n was found"
            case _ =>
              "Found applicable implicit"
          }
          def fullInfo  = "Found implicit: " + (if (e.coercion.isEmpty) "None" else e.coercion)
        }
    
      case e: InferImplicit =>
        new Descriptor {
          def kind = if (e.byName) "By-name implicits" else "Implicits"
          def basicInfo = kind + " search"
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            ("Implicits search for expression tree %tree" +
             "of type %tpe and expected type %tpe\n" +
             "Undetermined type parameters: %tpe").dFormat(Some(kind + " search"),
             anyString(tree1), snapshotAnyString(tree1.tpe), snapshotAnyString(e.pt),
             if (e.undetParams.isEmpty) "None" else e.undetParams.map(snapshotAnyString).mkString("[", ",", "]"))
          }
        }
       
      case e: ImprovesCompare =>
        new Descriptor {
          def basicInfo = "Compare two implicits"
          def fullInfo  = "Comparing implicit:\n " + e.info1 + "\n vs. \n" + e.info2
        }
         
      case e: ImprovesResult =>
        new Descriptor {
          def basicInfo = if (e.res) "Implicit improves" else "No improvement exists"
          def fullInfo  = ""
        }

      case e: VerifyImplicit =>
        new Descriptor {
          def basicInfo = "Verify available implicit"
          def fullInfo  = "Try implicit " + e.info.name + // TODO 
            "\nfor tree: " + snapshotAnyString(e.newTree) + "\nwith expected type " + snapshotAnyString(e.pt)
        }

      case e: ImplicitSearchDone =>
        DEFAULT

      case e: SearchContextImplicits =>
        new Descriptor {
          def basicInfo = "Search for applicable implicits\nin the scope"
          def fullInfo  = {
            val implicits = e.allImplicits.map(_.map(info => snapshotAnyString(info.sym)).mkString("[", ",", "]")).mkString("\n")
            "Search appropriate implicits in the current scope\n" + 
            "For tree " + snapshotAnyString(e.tree) + "\n" +
            "All available implicits in the context:\n" + implicits
          }
        }
        
      case e: ManifestAndExpectedTpeImplicits =>
        new Descriptor {
          def basicInfo = "Implicit manifests or expected type"
          def fullInfo  = "Search appropriate manifest evidence or implicit for the expected type " +
            snapshotAnyString(e.pt)
          
        }
         
      case e: SearchExpectedTypeImplicits =>
        new Descriptor {
          def basicInfo = "Search for implicits based on\nexpected type"
          def fullInfo  = "Search implicits based on\n" +
            "expected type '" + snapshotAnyString(e.pt) + "'.\n" +
            "These are all implicits found in companion objects of classes C\n" +
            "such that some part of expected type has C as of its super-classes."
        }
         
      case e: SearchManifestImplicits =>
        new Descriptor {
          def basicInfo = "Search evidence for manifest"
          def fullInfo  = "Search implicit for the manifest for type " + snapshotAnyString(e.tpe)
        }
         
      case e: AllEligibleImplicits =>
        new Descriptor {
          def basicInfo = "Filter all eligible implicits"
          def fullInfo  = "Filter all eligible implicits for expected type " + snapshotAnyString(e.pt)
        }
      
      case e: AllEligibleImplicitsDone =>
        new Descriptor {
          def basicInfo = "Filtered all eligible implicits"
          def fullInfo  = ""
        }
        
      case e: InfoEligibleTest =>
        new Descriptor {
          def basicInfo = "Test implicit eligibility\n" + e.info.name
          def fullInfo  = "Implicit: " + e.info.name
        }
        
      case e: InfoEligibleTestDone =>
        new Descriptor {
          def basicInfo = if (e.eligible) "Implicit eligible" else "Implicit not eligible"
          def fullInfo  = ""
        }
         
      case e: CheckTypesCompatibility =>
        new Descriptor {
          def basicInfo = "Check types compatibility " + (if (e.fast) "(quick)" else "")
          def fullInfo  = "Check compatibility of types: \n" + snapshotAnyString(e.tp) +
            " vs. " + snapshotAnyString(e.pt)
        }
        
      case e: CheckedTypesCompatibility =>
        new Descriptor {
          def basicInfo = if (e.res) "Types compatible" else "Types not compatible"
          def fullInfo  = ""
        }
        
      case e: AmbiguousImplicitsError =>
        new Descriptor {
          def basicInfo = "Ambiguous implicits"
          def fullInfo  = "Cannot apply implicit conversion due to ambiguity of two implicits:\n" +
            snapshotAnyString(e.info1Sym) + " of type " + snapshotAnyString(e.info1Tpe) + " and\n" + 
            snapshotAnyString(e.info2Sym) + " of type " + snapshotAnyString(e.info2Tpe)
        }

      case e: PossiblyValidImplicit =>
        new Descriptor {
          def basicInfo = if (e.result) "Valid" else "Invalid" + " implicit"
          def fullInfo  = if (e.result)
              "Typechecked valid implicit converstion:\n" + snapshotAnyString(e.sym) + " of type " + snapshotAnyString(e.tpe)
            else 
              "Invalid implicit converstion:\n" + snapshotAnyString(e.sym) + " of type " + snapshotAnyString(e.tpe)
        }

      case e: CyclicReferenceInImplicitsImprove =>
        new Descriptor {
          def basicInfo = "Cyclic reference while trying to improve implicit"
          def fullInfo  = ""
        }
        
      case e: DivergentImplicitEvent =>
        new Descriptor {
          def basicInfo = "Divergent implicit error"
          def fullInfo  = ""
        }
      
      case _ => DEFAULT

    }
  }
}