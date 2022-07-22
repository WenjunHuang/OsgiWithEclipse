package org.foo.paint

import java.awt.{BorderLayout, Color, Dimension}
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import javax.swing.{JFrame, JPanel, JToolBar}

class PaintFrame extends JFrame("PaintFrame") with MouseListener with MouseMotionListener:
  val toolBar = new JToolBar("Toolbar")
  val panel = new JPanel()
  panel.setBackground(Color.WHITE)
  panel.setLayout(null)
  panel.setMinimumSize(Dimension(400,400))
  panel.addMouseListener(this)
  getContentPane.setLayout(BorderLayout())
  getContentPane.add(toolBar,BorderLayout.NORTH)
  getContentPane.add(panel,BorderLayout.CENTER)

  override def mouseClicked(e: MouseEvent): Unit = ???

  override def mousePressed(e: MouseEvent): Unit = ???

  override def mouseReleased(e: MouseEvent): Unit = ???

  override def mouseEntered(e: MouseEvent): Unit = ???

  override def mouseExited(e: MouseEvent): Unit = ???

  override def mouseDragged(e: MouseEvent): Unit = ???

  override def mouseMoved(e: MouseEvent): Unit = ???

  setSize(400,400)


