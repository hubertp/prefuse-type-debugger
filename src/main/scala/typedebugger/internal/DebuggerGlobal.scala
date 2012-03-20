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
  outer: Global with DebuggerCompilationUnits with interactive.RangePositions =>
    
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
  
  // api
  
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
  
  def targetDebugAt(pos: Position) {
    // pos.source unit has always been typechecked before
    // assert: pos is valid!
    println("[debugging] at: " + pos)
    
    // locate the tree
    reloadSource(pos.source)               // flush source info
    globalPhase = _compilerRun.typerPhase  // if run uses typeCheck this will be no longer necessary
    val unit = unitOfFile(pos.source.file) // getorelse
    parseAndEnter(unit)
    // todo: avoid debugging if the the new tree is contained in the old one
    unit.targetPos = pos
    try {
      withInstrumentingOn {
        _compilerRun.typeCheck(unit)
      }
    } finally {
      unit.targetPos = NoPosition
    }
  }
  
  def parseAndEnter(unit: DebuggerCompilationUnit) {
    if (unit.status == NotLoaded) {
      _compilerRun.compileLate(unit)
      validatePositions(unit.body) // needed?
      // todo: syncTopLevelSyms 
      unit.status = JustParsed
    }
  }

}