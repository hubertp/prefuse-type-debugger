package scala.typedebugger
package ui

import javax.swing.{JPanel, Timer, JComponent,
         JLabel, JButton, BorderFactory,
         JTextArea, JScrollPane}
import java.awt.event.{MouseAdapter, MouseEvent, ActionListener, ActionEvent}
import java.awt.{Color, Font, BorderLayout, Insets}
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter
import javax.swing.border._

import scala.collection.mutable
import util.StringFormatter

trait PrefuseTooltips {
  abstract class PrefuseTooltip(
      startDelay: Int, stopDelay: Int,
      owner: JComponent, var sticky: Boolean) {
      
    protected var popup: JPanel = _
    
    protected def getContents(): java.awt.Component
    
    class ShowTimerAction(showTimer: Boolean) extends ActionListener {
      override def actionPerformed(e: ActionEvent ) =
        if(showTimer) startShowingImmediately() else stopShowingImmediately()
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
    
    def stopShowing() =
      if(popup.isVisible() && !sticky) stopShowingTimer.start() else startShowingTimer.stop()
    
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
    
    def bringToFront() =  popup.getParent().setComponentZOrder(popup, 0)
    
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
  
  class NodeTooltip(contents: util.StringFormatter, startDelay: Int, stopDelay: Int, owner: JComponent)
    extends PrefuseTooltip(startDelay, stopDelay, owner, false) {
    val contentsPanel = new JPanel()
    final val constTextFact = 1.5
    final val newLine = System.getProperty("line.separator")
    
    init()
    
    def init() {
      val textArea = swingFormattedText(contents, findAreaMaxHeight(), findAreaMaxWidth())
      textArea.setFont(new Font("monospaced", Font.PLAIN, 12))
      textArea.setEditable(false)
      textArea.setLineWrap(true)

      contents.title match {
        case Some(title) =>
          val titleBorder = BorderFactory.createTitledBorder(title)
          titleBorder.setTitleJustification(TitledBorder.LEFT)
          contentsPanel.setBorder(BorderFactory.createCompoundBorder(
                        titleBorder,
                        BorderFactory.createEmptyBorder(2, 2, 2, 2)))
        case None        =>
          contentsPanel.setBorder(BorderFactory.createLineBorder(Color.gray));
          contentsPanel.setLayout(new BorderLayout())
      }
      contentsPanel.add(new JScrollPane(textArea))
    }
    
    private def findAreaMaxWidth(areaMax: Int = 80, margin: Int = 2): Int = {
      var maxWidth = 0
      contents.text foreach { elem =>
        val localMax = (elem.split(newLine).max.length * constTextFact).toInt
        if (localMax > maxWidth)
          maxWidth = localMax min areaMax
      }
      contents.args foreach { elem =>
        val argMax = (elem.toString.split(newLine).max.length * constTextFact).toInt
        if (argMax > maxWidth)
          maxWidth = argMax min areaMax
      }
      maxWidth + margin
    }
    
    private def findAreaMaxHeight(areaMax: Int = 10, margin: Int = 2): Int = {
      val maxHeight = contents.text map (_.split(newLine).length) sum
      
      val totalArgsHeight = contents.args.map(
        _.text.split(newLine).length 
      ) sum
      
      val height = ((maxHeight + totalArgsHeight) * constTextFact) toInt
      
      margin + 
       (if (height > areaMax) areaMax else height)
    }
    
    private def swingFormattedText(formatter: StringFormatter, height: Int, width: Int): JTextArea = {
      val positions = new mutable.ListBuffer[(Int, Int)]()
      val color = new DefaultHighlightPainter(Color.lightGray)
      
      def addNewLine(s: String): String = {
        if (!s.endsWith(newLine)) s + newLine else s
      }
      
      var text = (formatter.text zip formatter.args).foldLeft("") { (x, y) =>
        val upto = x + addNewLine(y._1) // only text
        val withArg = upto + y._2 // tree/tpe/symbol
        positions += ((upto.length, withArg.length))
        addNewLine(withArg)
      }
      val textArea =  new JTextArea(height, width)
      textArea.setMargin(new Insets(2, 2, 2, 2));
      if (formatter.text.length != formatter.args.length) // add missing bit after zip
        text = text + addNewLine(formatter.text.last)
      textArea.setText(text)
      positions.foreach(poss => textArea.getHighlighter.addHighlight(poss._1, poss._2, color))
      textArea
    }
  
    protected def getContents() = contentsPanel
  }
}
