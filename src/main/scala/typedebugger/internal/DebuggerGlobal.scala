package scala.typedebugger
package internal

import scala.tools.nsc.interactive
import scala.tools.nsc.event.EventsGlobal
import scala.tools.nsc.Global
import scala.tools.nsc.event.HookLoader
import scala.tools.nsc.io
import scala.tools.nsc.util.{ SourceFile, BatchSourceFile }

import scala.collection.mutable

trait DebuggerGlobal extends EventsGlobal {
  outer: Global with DebuggerCompilationUnits with interactive.RangePositions with DebuggerPositions =>
    
  val unitOfFile = new mutable.LinkedHashMap[io.AbstractFile, DebuggerCompilationUnit]()
  
  trait DebuggerStrings extends Strings {
    self: EventModel =>
      
    // TODO provide custom printing
    abstract override protected def anyStringInternal(x: Any): String = x match {
      case x: Tree => super.treeString(x)
      case _       => super.anyStringInternal(x)
    }
  }
  
  override def EVGlobal: EventModel with EventPostInit = new EVGlobal with DebuggerStrings {
    override def instrumentingOn = instrumenting
  }
  
  // since overriding settings with more specific type won't work
  def debuggerSettings: scala.tools.nsc.Settings with TypeDebuggerSettings
  
  // optional approach to target debugging, when we abort further typechecking
  // once we are done with the searched for tree `result`
  override def signalDone(context: analyzer.Context, old: Tree, result: Tree) {
    val target = context.unit.targetPos
    if (debuggerSettings.withTargetThrow.value && target != NoPosition && context.unit.exists
        && result.pos.isOpaqueRange && (result.pos includes context.unit.targetPos)) {
      if (debuggerSettings.debugTD.value)
        println("target debugging, typechecked required tree, abort")
      val located = new Locator(target) locateIn result
      if (located != EmptyTree)
        throw new EV.TargetDebugDone(result)
    }
  }
  
  private var _instrumenting = false
  def instrumenting = _instrumenting
  
  def withInstrumentingOn[T](f: => T): T = {
    val save = instrumenting
    _instrumenting = true
    try {
      f
    } finally {
      _instrumenting = save
    }
  }
  
  // compilation api
  private var _compilerRun: DebuggerRun = _
  
  def initRun() {
    _compilerRun = new DebuggerRun
  }
  
  initRun()
  
  class DebuggerRun extends Run {
    override def canRedefine(sym: Symbol) = true // does this affect typechecking info produced
    
    def typeCheck(unit: CompilationUnit): Unit = {
      atPhase(typerPhase) { typerPhase.asInstanceOf[GlobalPhase] applyPhase unit }
    }
  }
  
  def run(files: List[io.AbstractFile]) {
    unitOfFile.clear()
    
    val units = files map {f => 
      val batchFile = new BatchSourceFile(f)
      val unit = new DebuggerCompilationUnit(batchFile)
      unitOfFile(f) = unit
      unit
    }
    // use parseAndEnter
    // and then typeCheck for each unit
    withInstrumentingOn { // todo: turn off
      _compilerRun.compileUnits(units)
    }
  }
  
  // some of the code here is a duplicate of interactive.Global
  // but we cannot/do not want to use it, so that is acceptable
  def reloadSource(source: SourceFile) {
    val unit = new DebuggerCompilationUnit(source)
    unitOfFile(source.file) = unit
    reset(unit)
  }
  
  private def reset(unit: DebuggerCompilationUnit) {
    unit.depends.clear()
    unit.defined.clear()
    unit.synthetics.clear()
    unit.toCheck.clear()
    unit.targetPos = NoPosition
    unit.body = EmptyTree
  }
  
  def locate(pos: Position): Tree = {
    val unit = unitOfFile(pos.source.file)
    new Locator(pos) locateIn unit.body
  }
  
  def locateStatement(pos: Position): Tree = {
    val unit = unitOfFile(pos.source.file)
    new StatementLocator(pos) locateStat unit.body
  }
  
  def targetDebugAt(pos: Position) {
    // pos.source unit has always been typechecked before
    // assert: pos is valid!
    println("[debugging] at: " + pos)
    
    // locate the tree
    reloadSource(pos.source)               // flush source info
    globalPhase = _compilerRun.typerPhase  // if run uses typeCheck this will be no longer necessary
    val unit = unitOfFile(pos.source.file)
    parseAndEnter(unit)
    // todo: avoid debugging if the the new tree is contained in the old one
    unit.targetPos = pos
    try {
      withInstrumentingOn {
        _compilerRun.typeCheck(unit)
      }
      // ensure that this can only happen if we do not throw an exception
      if (debuggerSettings.withTargetThrow.value)
        println("[bug] should fully typecheck tree when throwing error on targeted debugging")
    } catch {
      case ex: EV.TargetDebugDone =>
        if (!debuggerSettings.withTargetThrow.value)
          println("[bug] shouldn't end prematurely typechecking on targeted debugging") 
    } finally {
      unit.targetPos = NoPosition
    }
  }
  
  def parseAndEnter(unit: DebuggerCompilationUnit) {
    _compilerRun.compileLate(unit)
    validatePositions(unit.body) // needed?
    // todo: syncTopLevelSyms 
  }

}