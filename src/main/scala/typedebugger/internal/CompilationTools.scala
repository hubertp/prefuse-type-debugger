package scala.typedebugger
package internal

import scala.tools.nsc.io

trait CompilationTools {
  self: CompilerInfo with IStructure =>
    
  trait CompilerRunWithEventInfo {
    def run(srcs: List[io.AbstractFile]): CompilerRunResult
    def runTargeted(pos: global.Position, expandPos: global.Position): CompilerRunResult
  }
  
  trait CompilerRunResult {
    def root: BaseTreeNode[EventNode]
    def goals: List[BaseTreeNode[EventNode]]
  }
}