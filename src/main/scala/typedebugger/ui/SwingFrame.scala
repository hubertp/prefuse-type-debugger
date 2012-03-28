package scala.typedebugger
package ui

import java.awt.BorderLayout
import java.awt.event.{WindowAdapter, WindowEvent, ItemListener, ItemEvent}
import javax.swing.{Action => swingAction, _}

import scala.concurrent.Lock
import scala.tools.nsc.io

class SwingFrame(prefuseComponent: PrefuseComponent,
                          frameName: String,
                          filtState: Boolean) {

  val jframe = new JFrame(frameName)
  val topPane = new JPanel(new BorderLayout())

  val ASTViewer = new JTextArea(30, 90)
  val sCodeViewer = new JTextArea(30, 30)
  val infoViewer = new JTextArea(2, 5)

  def createFrame(lock: Lock): Unit = {
    lock.acquire // keep the lock until the user closes the window
    jframe.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    jframe.addWindowListener(new WindowAdapter() {
      override def windowClosed(e: WindowEvent): Unit = lock.release
    })

    val tabFolder = new JTabbedPane()
    // Split right part even further
    val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, prefuseComponent, new JScrollPane(tabFolder))
    topSplitPane.setResizeWeight(0.7)
    
    topPane.add(topSplitPane)
    val codeViewPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, new JScrollPane(sCodeViewer), infoViewer)
    codeViewPane.setResizeWeight(1.0)
    codeViewPane.setDividerLocation(0.1)
    tabFolder.addTab("Tree", null, codeViewPane)
    sCodeViewer.setEditable(false)
    tabFolder.addTab("Transformed tree", null, new JScrollPane(ASTViewer))
    
    // add menu
    val menuBar = new JMenuBar()
    val viewMenu = new JMenu("View")

    // dynamic filtering
    val filtering = new JMenu("Event filtering")
    addFilteringOptions(filtering)
    
    viewMenu.add(filtering)
    
    menuBar.add(viewMenu)
    
    
    jframe.setJMenuBar(menuBar)
    jframe.getContentPane().add(topPane)
    jframe.pack()
    jframe.setVisible(true)
  }

  def addFilteringOptions(parent: JMenu) {
    Filtering.values foreach { v =>
      val item = new JCheckBoxMenuItem(v.toString)
      if (filtState) {
        item.setState(filtState)
        prefuseComponent.adv.enableOption(v)
      }
      item.addItemListener(filteringBoxListener)
      parent.add(item)
    }
  }
  
  def filteringBoxListener: ItemListener = FilteringListener
  private object FilteringListener extends ItemListener {
    def itemStateChanged(e: ItemEvent) {
      val checkItem = e.getItem.asInstanceOf[JCheckBoxMenuItem]
      val option = Filtering.withName(checkItem.getText)
      if (e.getStateChange() == ItemEvent.SELECTED) {
        prefuseComponent.adv.enableOption(option)
      } else {
        prefuseComponent.adv.disableOption(option)
        prefuseComponent.reRenderDisabledEvents()
      }
      prefuseComponent.reRenderView()
    }
  }

}