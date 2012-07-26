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
          def basicInfo = if (e.coercionFound) "Found applicable implicit"
                          else "No applicable implicit\n was found"
          def fullInfo  = if (e.coercionFound) "Found coercion %tree".dFormat(anyString(e.coercion)) else ""
        }
    
      case e: InferImplicit =>
        new Descriptor {
          def kind = if (e.byName) "a by-name implicit" else "an implicit"
          def tpeKind = if (definitions.isFunctionType(e.pt)) "view" else "value"
          def basicInfo = "Search for " + kind + " " + tpeKind
          def fullInfo  = {
            val tree1 = treeAt(e.tree)
            if (tree1 == EmptyTree)
              "Searching for a view of type %tpe".dFormat(Some(basicInfo + " view"), snapshotAnyString(e.pt))
            else
              ("Search for an implicit for an expression tree %tree" +
               "of type %tpe and expected type %tpe\n" +
               "Undetermined type parameters: %tpe").dFormat(Some(basicInfo),
               anyString(tree1), snapshotAnyString(tree1.tpe), snapshotAnyString(e.pt),
               if (e.undetParams.isEmpty) "None" else e.undetParams.map(snapshotAnyString).mkString("[", ",", "]"))
          }
        }
       
      case e: ImprovesCompare =>
        new Descriptor {
          def basicInfo = "Compare two implicits"
          def fullInfo  = "Comparing implicits %sym vs. %sym".dFormat(e.info1.toString, e.info2.toString)
        }
         
      case e: ImprovesResult =>
        new Descriptor {
          def basicInfo = if (e.res) "Implicit improves" else "No improvement exists"
          def fullInfo  = ""
        }

      case e: VerifyImplicit =>
        new Descriptor {
          def withName = {
            visibleName(e.info.name) match {
              case Some(str) => " " + str
              case None      => ""
            }
          }
          def basicInfo = "Verify available implicit" + withName
          def fullInfo  = "Can we apply implicit %sym to expression %tree given expected type %tpe".dFormat(
              Some("Verifying implicit"), e.info.name.toString, snapshotAnyString(e.newTree),
              snapshotAnyString(e.pt))
        }

      case e: ImplicitSearchDone =>
        DEFAULT

      case e: SearchContextImplicits =>
        new Descriptor {
          def basicInfo = "Is there a matching implicit\n in the scope?"
          def fullInfo  = {
            val implicits = e.allImplicits.map(_.map(info => snapshotAnyString(info.sym)).mkString("[", ",", "]")).mkString("\n")
            ("Search appropriate implicits in the current scope.\n" + 
            "For tree %tree" +
            "All available implicits in the context: %tpe").dFormat(Some("Search for implicits in the scope"),
             snapshotAnyString(e.tree), implicits)
          }
        }
         
      case e: SearchExpectedTypeImplicits =>
        new Descriptor {
          def basicInfo = "Is there a matching implicit\n corresponding to the expected type?"
          def fullInfo  = ("Search implicits based on\n" +
            "expected type %tpe \n" +
            "These are all implicits found in companion objects of classes C\n" +
            "such that some part of expected type has C as of its super-classes.").dFormat(
            Some("Search for implicit based on the expected type"),
            snapshotAnyString(e.pt))
        }
         
      // todo
      case e: SearchManifestImplicits =>
        new Descriptor {
          def basicInfo = "Search evidence for manifest"
          def fullInfo  = "Search implicit for the manifest for type " + snapshotAnyString(e.tpe)
        }
         
      case e: AllEligibleImplicits =>
        new Descriptor {
          def basicInfo = "What are the potentially eligible implicits?"
          def fullInfo  = "Filter all eligible implicits for expected type %tpe".dFormat(snapshotAnyString(e.pt))
        }
      
      case e: AllEligibleImplicitsDone =>
        new Descriptor {
          def basicInfo = "Filtered all eligible implicits"
          def fullInfo  = ""
        }
        
      case e: InfoEligibleTest =>
        new Descriptor {
          def basicInfo = "Is '" + e.info.name + "' implicit eligible?"
          def fullInfo  = "Implicit: %sym\nType: %tpe".dFormat(
              Some("Implicit eligibility test"), e.info.name.toString, anyString(e.info.sym.tpe))
        }
        
      case e: InfoEligibleTestDone =>
        new Descriptor {
          def basicInfo = if (e.eligible) "Implicit eligible" else "Implicit not eligible"
          def fullInfo  = ""
        }
        
      case e: CheckedTypesCompatibility =>
        new Descriptor {
          def basicInfo = if (e.res) "Types compatible" else "Types not compatible"
          def fullInfo  = ""
        }
        
      case e: AmbiguousImplicitsError =>
        new Descriptor {
          def basicInfo = "Ambiguous implicits"
          def fullInfo  = ("Cannot apply implicit conversion due to ambiguity of two implicits:\n" +
             "%sym of type %tpe and\n %sym of type %tpe").dFormat(Some("Ambiguous implicits"), 
            snapshotAnyString(e.info1Sym), snapshotAnyString(e.info1Tpe),
            snapshotAnyString(e.info2Sym), snapshotAnyString(e.info2Tpe))
        }

      case e: PossiblyValidImplicit =>
        new Descriptor {
          def basicInfo = if (e.result) "Valid" else "Invalid" + " implicit"
          def fullInfo  = (if (e.result)
              "Typechecked valid implicit converstion %sym of type %tpe"
            else 
              "Invalid implicit converstion %sym of type %tpe").dFormat(snapshotAnyString(e.sym), 
               snapshotAnyString(e.tpe))
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