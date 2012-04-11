package scala.typedebugger
package processing

trait Hooks {
  self: internal.CompilerInfo with internal.PrefuseStructure =>
    
  trait PostCompilationHook {
    def info(goals: List[UINode[PrefuseEventNode]]): Unit
  }
  
  def targetedCompile(pos: global.Position, hook: PostCompilationHook): Unit
}