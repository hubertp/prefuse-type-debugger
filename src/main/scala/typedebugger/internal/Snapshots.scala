package scala.typedebugger
package internal

trait Snapshots { self: scala.reflect.internal.SymbolTable =>
  
  def treeAt(tree: Tree, time: Clock): Tree = {
    new TreeSnapshot(time).transform(tree) 
  }
  
  class TreeSnapshot(time: Clock) extends Transformer {
    lazy val strictCopier = newStrictTreeCopier
    
    private def attributesAt(tree: Tree): (Type, Symbol) = {      
      def findMissingAttr[T](startElem: AttributesHistory, default: T): T = {
        // assert startElem != null
        var treeAttr = startElem.prev
        while (treeAttr != null && treeAttr.m <:< startElem.m) // this assumes that we only deal with 
          treeAttr = treeAttr.prev
        
        if (treeAttr == null) default
        else treeAttr.v.asInstanceOf[T]
      }
      
      var attr0 = tree.attributes
      // find the last change (either sym or tpe)
      // and then find the missing bit (either sym or tpe)
      while (attr0 != null && time < attr0.clock) {
        attr0 = attr0.prev
      }
      
      if (attr0 == null) (NoType, NoSymbol)
      else attr0 match {
        case TreeTpeChange(_, tpe, _) =>
          (tpe, findMissingAttr(attr0, NoSymbol))
        case TreeSymChange(_, sym, _) =>
          (findMissingAttr(attr0, NoType), sym)
      }
    }
    
    def snapshot(tree: Tree): Option[(Type, Symbol)] = {
      if (tree != null) {
        val (tp0, sym0) = attributesAt(tree)
      
        if ((tp0 eq tree.tpe) && (sym0 eq tree.symbol)) None
        else Some((tp0, sym0))
      } else None
    }
    
    override def transform(tree: Tree): Tree = {
      val t1 = super.transform(tree)
      
      val t = snapshot(tree) match {
        case None        => t1
        case Some(attrs) =>
          // strict copy the tree (outer layer only)
          val t2 = t1.shallowDuplicate
          t2.setTypeNoLog(attrs._1)
          if (tree.hasSymbol) t2.setSymbolNoLog(attrs._2)
          t2
      }
      t setPos makeTransparent(tree.pos)
      t
    }
  }
  
  object TypeSnapshot extends ((Type, Clock) => Type) {
    def apply(tp: Type, time: Clock): Type = tp match {
      case TypeRef(pre, sym, args) =>
        val pre1 = this(pre, time)
        val sym1 = SymbolSnapshot(sym, time)
        val args1 = args.mapConserve(this(_, time))
        if ((pre1 eq pre) && (sym1 eq sym) && (args1 eq args)) tp
        else TypeRef(pre1, sym1, args1)
      case ThisType(sym) =>
        val sym1 = SymbolSnapshot(sym, time)
        if (sym1 eq sym) tp
        else ThisType(sym1)
      case SingleType(pre, sym) =>
        val pre1 = this(pre, time)
        val sym1 = SymbolSnapshot(sym, time)
        if ((pre1 eq pre) && (sym1 eq sym)) tp
        else SingleType(pre1, sym1)
      case MethodType(params, restpe) =>
        val params1 = params.mapConserve(SymbolSnapshot(_, time))
        //println("Method Type: " + restpe.getClass + " params: " + params.map(_.getClass))
        val restpe1 = this(restpe, time)
        //println("METHOD TYPE SNAPSHOT: " + (params1 eq params) + " && " + (restpe1 eq restpe))
        if ((params1 eq params) && (restpe1 eq restpe)) tp
        else MethodType(params1, restpe1)
      case PolyType(tparams, restpe) =>
        val tparams1 = tparams.mapConserve(SymbolSnapshot(_, time))
        val restpe1 = this(restpe, time)
        if ((tparams1 eq tparams) && (restpe1 eq restpe)) tp
        else PolyType(tparams1, restpe1)
      case NullaryMethodType(result) =>
        val result1 = this(result, time)
        if (result1 == result) tp
        else NullaryMethodType(result1)
/*      case ConstantType(const) => // todo
        val constTpe = this(const.tpe, time)
        if (constTpe eq const.tpe) tp
        else ... */
      case SuperType(thistp, supertp) =>
        val thistp1 = this(thistp, time)
        val supertp1 = this(supertp, time)
        if ((thistp1 eq thistp) && (supertp1 eq supertp)) tp
        else SuperType(thistp1, supertp1)
      case TypeBounds(lo, hi) =>
        val lo1 = this(lo, time)
        val hi1 = this(hi, time)
        if ((lo1 eq lo) && (hi1 eq hi)) tp
        else TypeBounds(lo1, hi1)
      case BoundedWildcardType(bounds) =>
        val bounds1 = this(bounds, time)
        if (bounds1 eq bounds) tp
        else BoundedWildcardType(bounds1.asInstanceOf[TypeBounds])
      case RefinedType(parents, decls) =>
        val parents1 = parents.mapConserve(this(_, time))
        // todo: handle decls
        if (parents1 eq parents) tp
        else RefinedType(parents1, decls)
      case ExistentialType(tparams, result) =>
        val tparams1 = tparams.mapConserve(SymbolSnapshot(_, time))
        val result1 = this(result, time)
        if ((tparams1 eq tparams) && (result1 eq result)) tp
        else ExistentialType(tparams1, result1)
      case AnnotatedType(annots, atp, selfsym) =>
        // todo: support annotations
        val atp1 = this(atp, time)
        val selfsym1 = SymbolSnapshot(selfsym, time)
        if ((atp1 eq atp) && (selfsym1 eq selfsym)) tp
        else AnnotatedType(annots, atp1, selfsym1)
      case OverloadedType(pre, alts) =>
        val pre1 = this(pre, time)
        val alts1 = alts.mapConserve(SymbolSnapshot(_, time))
        if ((pre1 eq pre) && (alts1 eq alts)) tp
        else OverloadedType(pre1, alts1)
      case NotNullType(result) =>
        val result1 = this(result, time)
        if (result1 eq result) tp
        else NotNullType(result1)
      case tv@TypeVar(_, _) =>
        // primary source of differences
        val tv1 = typeVarAt(tv, time)
        //println("TYPEVAR RESULT: " + (tv1 eq tv) + " for " + tv1 + " vs " + tv)
        if (tv1 eq tv) tp
        else tv1 
      // handle antipolytype
      //        annotatedtype
      //        debruijnindex
      case _ => tp
    }
    
    private def typeVarAt(tv: TypeVar, time: Clock): TypeVar = {
      def condTypeVarConstraint(constr0: TypeConstraint): TypeVar = {
        val constr1 = typeConstraintAt(constr0, time)
        if (constr1 eq constr0) tv
        else typeVarFactory(tv, constr1)
      }
      //println("type-var-at ["+time+"]: " + tv + " with snapshots " + tv.snapshot)
      
      if (tv.snapshot == null || tv.snapshot.clock < time) {
        condTypeVarConstraint(tv.constr)
      } else {
        var typevar0 = tv.snapshot
        while (typevar0 != null && time < typevar0.clock)
          typevar0 = typevar0.prev

        if (typevar0 == null) 
          condTypeVarConstraint(tv.constr0) // constr0 if available, otherwise we have to reconstruct it
        else
          condTypeVarConstraint(typevar0.constr)
      }
    }
    
    private def typeVarFactory(tv: TypeVar, constr: TypeConstraint) = tv match {
      case hktv: HKTypeVar => TypeVar(tv.origin, constr, Nil, hktv.params)
      case apptv: AppliedTypeVar => TypeVar(tv.origin, constr, apptv.typeArgs, apptv.params)
      case _ => TypeVar(tv.origin, constr)
    }
    
    private def typeConstraintAt(tc: TypeConstraint, time: Clock): TypeConstraint = {
      //println("type-constraint-at ["+time+"]: " + tc)
       
      // short-cut if clock points to the current type constraint
      if (tc.constrSnapshot == null) tc // todo should map also the init bounds
      else {
        var upTo = tc.constrSnapshot
        while (upTo != null && time < upTo.clock)
          upTo = upTo.prev
          
        // apply all the bounds in the reverse order
        // TODO: we should apply snapshots to each of the types as well
        // especially because init is a lazy val
        val newConstraint = new TypeConstraint(tc.init.lo, tc.init.hi, tc.init.numlo, tc.init.numhi)
        def applyChange(change: ConstrChange) { 
          if (change == null) () else {
            applyChange(change.prev)
            change match {
              case InstChange(_, inst, _) =>
                newConstraint.inst = inst
              case BoundChange(_, bound, lowerBound, isNumericBound, _) =>
                if (lowerBound)
                  newConstraint.addLoBound(bound, isNumericBound)
                else
                  newConstraint.addHiBound(bound, isNumericBound)
            }
          }
        }
        applyChange(upTo)
        newConstraint
      }
    }
  }

  object SymbolSnapshot extends ((Symbol, Clock)  => Symbol) {
    def apply(sym: Symbol, time: Clock): Symbol = {
      if (sym == null)
        println("warn: symbol is null")
      val info1 = infoAt(sym, time)
      if (info1 eq sym.info) sym
      else sym.cloneSymbol.setInfoNoLog(info1)
    }
    
    private def infoAt(sym: Symbol, at: Clock): Type = {
      var snapshot0 = sym.snapshot
      while (snapshot0 != null && at < snapshot0.clock)
        snapshot0 = snapshot0.prev
        
      if (snapshot0 == null) NoType
      else {
        if (!snapshot0.info.isComplete) {
          // if not complete, this can create bootstrapping issues 
          // where we for example we create Unit symbols that differ with info
          snapshot0 = sym.snapshot
          if (!snapshot0.info.isComplete) sym.info
          else {
            while (snapshot0.prev != null && snapshot0.prev.info.isComplete && at < snapshot0.clock)
              snapshot0 = snapshot0.prev
            // todo: convert with TypeSnapshot?
            snapshot0.info
          }
        } else 
          TypeSnapshot(snapshot0.info, at)
      }
    }
  }

}