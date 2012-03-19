package scala.typedebugger
package ui

import java.awt.BorderLayout
import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.{Action => swingAction, _}

import scala.concurrent.Lock
import scala.tools.nsc.io.{File => ScalaFile}

class SwingFrame(val prefuseComponent: PrefuseComponent, frameName: String, srcs: List[String]) {

  val frame = new JFrame(frameName)
  val topPane = new JPanel(new BorderLayout())

  val ASTViewer = new JTextArea(30, 90)
  val sCodeViewer = new JTextArea(30, 30)

  def createFrame(lock: Lock): Unit = {
    lock.acquire // keep the lock until the user closes the window
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.addWindowListener(new WindowAdapter() {
      override def windowClosed(e: WindowEvent): Unit = lock.release
    })

    val tabFolder = new JTabbedPane()
    // Split right part even further
    val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, prefuseComponent, new JScrollPane(tabFolder))
    topSplitPane.setResizeWeight(0.7)
    
    topPane.add(topSplitPane)
    tabFolder.addTab("Tree", null, new JScrollPane(sCodeViewer))
    tabFolder.addTab("Transformed tree", null, new JScrollPane(ASTViewer))


    if (srcs.isEmpty)
      println("[Warning] No files specified for debugging.")
    loadFile(srcs.head)
    frame.getContentPane().add(topPane)
    frame.pack()
    frame.setVisible(true)
  }

  private def loadFile(fName: String) {
    // at the moment we only ensure that there is only one
    val f = new java.io.File(fName)
    val src = if (f.exists) ScalaFile(fName).slurp else "Missing source code"
    sCodeViewer.setText(src)
  }

}