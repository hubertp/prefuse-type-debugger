package scala.typedebugger
package ui

import java.awt.{ BorderLayout, Dimension }
import java.awt.event.{WindowAdapter, WindowEvent, ItemListener,
                       ItemEvent, KeyListener, KeyEvent, KeyAdapter}
import javax.swing.{Action => swingAction, _}
import event.{ ChangeListener, ChangeEvent }

import scala.concurrent.Lock
import scala.tools.nsc.io
import scala.collection.mutable

abstract class SwingFrame(frameName: String, filtState: Boolean,
    srcs: List[io.AbstractFile]) {
  
  class ClosableFrame(name: String) extends JFrame(name) {
    def fireCloseEvent() = processWindowEvent(new WindowEvent(jframe, WindowEvent.WINDOW_CLOSING))
  }

  val jframe = new ClosableFrame(frameName)
  val topPane = new JPanel(new BorderLayout())

  val ASTViewer = new JTextArea(30, 90)
  val sCodeViewer = new JTextArea(30, 30)
  val statusBar = new JLabel()
  
  protected val tabDisplayFiles = new JTabbedPane()
  
  def processKeyEvent(k: KeyEvent): Unit
  def advController: AdvancedOptionsController
  def switchSources(display: PrefuseDisplay): Unit

  def createFrame(lock: Lock, detached: Boolean): Unit = {
    lock.acquire // keep the lock until the user closes the window
    jframe.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    jframe.addWindowListener(new WindowAdapter() {
      override def windowClosed(e: WindowEvent): Unit = lock.release
    })


    tabDisplayFiles.addKeyListener(new KeyAdapter() {
      var currentAltKey: Boolean = false
      // conflicts with our navigation for Left <-> Right in tabs,
      // so requires Alt
      override def keyPressed(k: KeyEvent): Unit = {
        k.getKeyCode match {
          case KeyEvent.VK_ALT =>
            currentAltKey = true
          case _ if currentAltKey =>
            processKeyEvent(k)
          case _ =>
        }
      }
      
      override def keyReleased(k: KeyEvent): Unit = {
        k.getKeyCode match {
          case KeyEvent.VK_ALT =>
            currentAltKey = false
          case _ =>
        }
      }
    })
    
    tabDisplayFiles.addChangeListener(new TabbedListener(0))
    

    
    val tabFolder = new JTabbedPane()
    tabFolder.addTab("Tree", null, new JScrollPane(sCodeViewer))
    sCodeViewer.setEditable(false)
    tabFolder.addTab("Transformed tree", null, new JScrollPane(ASTViewer))
    
    val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, tabDisplayFiles, new JScrollPane(tabFolder))
    topSplitPane.setResizeWeight(0.7)
    topPane.add(topSplitPane)
    
    // add menu
    val menuBar = new JMenuBar()
    val viewMenu = new JMenu("View")

    // dynamic filtering
    val filtering = new JMenu("Event filtering")
    addFilteringOptions(filtering)
    
    viewMenu.add(filtering)
    
    menuBar.add(viewMenu)
    
    statusBar.setPreferredSize(new Dimension(topPane.getWidth, 15))
    statusBar.setHorizontalAlignment(SwingConstants.LEFT)
    topPane.add(statusBar, BorderLayout.SOUTH)
    topPane.setBorder(new border.BevelBorder(border.BevelBorder.LOWERED))
    
    jframe.setJMenuBar(menuBar)
    jframe.getContentPane().add(topPane)
    jframe.pack()
    jframe.setVisible(true)
  }
  
  // todo: cache the result
  protected def currentDisplay[T <: PrefuseDisplay]: T =
    tabDisplayFiles.getSelectedComponent.asInstanceOf[T]

  class TabbedListener(startIndex: Int) extends ChangeListener {
    var selected = startIndex
    def stateChanged(change: ChangeEvent) {
      val pane = change.getSource.asInstanceOf[JTabbedPane]
      val newComponentId = pane.getSelectedIndex
      if (selected != newComponentId) {
        // fade out old display, fade in the new one
        val oldDisplay = pane.getComponentAt(selected).asInstanceOf[PrefuseDisplay]
        val display = pane.getComponentAt(newComponentId).asInstanceOf[PrefuseDisplay]
        oldDisplay.hideView()
        loadSourceFile(display.source)
        switchSources(display)
        display.reRenderView()
        selected = newComponentId
      }
    }
  }
  
  protected def loadSourceFile(absFile: io.AbstractFile) {
    val src = if (absFile.file.exists)
      io.File(absFile.file).slurp
    else "Missing source code"
   sCodeViewer.setText(src)
  }

  def addFilteringOptions(parent: JMenu) {
    val grouped = Filtering.values groupBy {
      v => v match {
        case g: Filtering.GroupVal =>
          g.group
        case _ =>
          Groups.NoGroup
      }
    }
    grouped foreach { g =>
      val groupMenu = new JMenu(g._1.toString)
      g._2 foreach { v =>
        val item = new JCheckBoxMenuItem(v.toString)
        if (filtState) {
          item.setState(filtState)
          advController.enableOption(v)
        }
        item.addItemListener(filteringBoxListener)
        groupMenu.add(item)
      }
      parent.add(groupMenu)
    }
  }
  
  def filteringBoxListener: ItemListener = FilteringListener
  private object FilteringListener extends ItemListener {
    def itemStateChanged(e: ItemEvent) {
      val checkItem = e.getItem.asInstanceOf[JCheckBoxMenuItem]
      val option = Filtering.withName(checkItem.getText)
      if (e.getStateChange() == ItemEvent.SELECTED) {
        advController.enableOption(option)
      } else {
        advController.disableOption(option)
        currentDisplay.reRenderDisabledEvents() // fixme: should do that for all displays?
      }
      currentDisplay.reRenderView()
    }
  }

}