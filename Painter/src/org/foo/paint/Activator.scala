package org.foo.paint

import org.osgi.framework.{BundleActivator, BundleContext}

import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.{JFrame, SwingUtilities, WindowConstants}

class Activator extends BundleActivator with Runnable:
  override def start(context: BundleContext): Unit =
    println("Paint started")
    bundleContext = Some(context)

    if SwingUtilities.isEventDispatchThread then run()
    else SwingUtilities.invokeAndWait(this)

  override def stop(context: BundleContext): Unit =
    println("Paint stopped")
    frame match
      case Some(f) =>
        SwingUtilities.invokeLater(() =>
          f.setVisible(false)
          f.dispose()
        )
      case None =>
    frame = None
    bundleContext = None

  override def run(): Unit =
    val newFrame = new JFrame()
    newFrame.setTitle("Fucked You")
    newFrame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    newFrame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit =
        bundleContext match
          case Some(ctx) => ctx.getBundle(0).stop()
          case None =>
    })

    newFrame.setVisible(true)
    frame = Some(newFrame)

  var frame: Option[JFrame] = None
  var bundleContext: Option[BundleContext] = None
