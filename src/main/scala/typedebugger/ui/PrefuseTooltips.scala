package scala.typedebugger
package ui

import javax.swing.{JPanel, Timer, JComponent,
         JLabel, JButton, BorderFactory,
         JTextArea, JScrollPane}
import java.awt.event.{MouseAdapter, MouseEvent, ActionListener, ActionEvent}
import java.awt.{Color, Font, BorderLayout}

trait PrefuseTooltips {
  abstract class PrefuseTooltip(
      startDelay: Int, stopDelay: Int,
      owner: JComponent, var sticky: Boolean) {
      
    protected var popup: JPanel = _

    
    class ShowTimerAction(showTimer: Boolean)
      extends ActionListener
    {
      override def actionPerformed(e: ActionEvent ) {
        if(showTimer)
          startShowingImmediately()
        else
          stopShowingImmediately()
      }
    }
    
    val startShowingTimer = new Timer(startDelay, new ShowTimerAction(true))
    val stopShowingTimer = new Timer(stopDelay, new ShowTimerAction(false))
          
    startShowingTimer.setRepeats(false)
    stopShowingTimer.setRepeats(false)

    def startShowing(x: Int, y: Int, invertX: Boolean, invertY: Boolean) {
      val contents: java.awt.Component  = getContents()
      contents.addMouseListener(new PrefuseTooltipListener())
  
      popup = new JPanel(new java.awt.BorderLayout(), true)
      popup.setVisible(false)
      val dim = contents.getPreferredSize()
      val x0 = if (invertX) x - dim.width else x + 5
      val y0 = if (invertY) y - dim.height - 5 else y + 5

      popup.setLocation(if (x0 < 5) 5 else x0, if (y0 < 5) 5 else y0)
      popup.setSize(contents.getPreferredSize())
      popup.add(contents, java.awt.BorderLayout.CENTER)
      owner.add(popup)
  
      startShowingTimer.start()
    }
    
    def stopShowing() {
      if(popup.isVisible() && !sticky)
        stopShowingTimer.start()
      else
        startShowingTimer.stop()
    }
    
    def startShowingImmediately() {
      stopShowingTimer.stop()
      if(!popup.isVisible()) {
        startShowingTimer.stop()
        bringToFront()
        popup.setVisible(true)
      }
    }
    
    def stopShowingImmediately() {
      startShowingTimer.stop()
      if(popup.isVisible() && !sticky) {
        stopShowingTimer.stop()
        popup.setVisible(false)
      }
    }
    
    def bringToFront() {
      popup.getParent().setComponentZOrder(popup, 0)
    }
    
    protected def getContents(): java.awt.Component
    
   
    
    class PrefuseTooltipListener extends MouseAdapter {
      override def mouseEntered(e: MouseEvent) {
        stopShowingTimer.stop()
      }
      
      override def mouseExited(e: MouseEvent) {
        if((e.getSource()).asInstanceOf[java.awt.Component].getMousePosition() != null) {
          // don't stop showing if we are still inside the contents.
          // this is to fix the "feature" where mouseExited is fired when
          // the cursor is moved over a child component of contents. eg,
          // when the cursor is moved onto a JButton that is inside a
          // JPanel contents box, etc.
          return
        }
        
        //println("Mouse exited : " + e)
        stopShowing()
      }
    }

  }
  
  class NodeTooltip(name: String, contents: String, startDelay: Int, stopDelay: Int, owner: JComponent)
    extends PrefuseTooltip(startDelay, stopDelay, owner, false) {
    val contentsPanel = new JPanel(new BorderLayout())
    
    init()
    
    def init() {
      val nameLabel = new JLabel()
      nameLabel.setText(name)
      
      // Should dynamically adjust
      val textArea =  new JTextArea(10, 80)
      textArea.setText(contents)
      textArea.setFont(new Font("monospaced", Font.PLAIN, 15))
      textArea.setEditable(false)
      textArea.setLineWrap(true)
      contentsPanel.setBackground(new Color(255, 250, 205));
    
      contentsPanel.add(nameLabel)
      contentsPanel.add(new JScrollPane(textArea))

      contentsPanel.setBorder(BorderFactory.createLineBorder(Color.gray));
      contentsPanel.setBackground(new Color(255, 250, 205));
    }
  
    protected def getContents() = contentsPanel
  }
}
