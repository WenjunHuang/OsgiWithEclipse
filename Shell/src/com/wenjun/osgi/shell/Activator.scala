package com.wenjun.osgi.shell

import org.osgi.framework.{BundleActivator, BundleContext}

import scala.io.StdIn

class Activator extends BundleActivator:
  override def start(context: BundleContext): Unit =
    println(
      s"Bundle: ${context.getBundle.getSymbolicName} started with bundle id ${context.getBundle.getBundleId} on thread: ${Thread.currentThread().getId}"
    )
    val t = new Thread(CommandRunnable(context))
    t.setDaemon(true)
    t.start()
    thread = Some(t)

  override def stop(context: BundleContext): Unit =
    thread match
      case None =>
      case Some(t) =>
        t.interrupt()
        thread = None

  var thread: Option[Thread] = None

class CommandRunnable(val context: BundleContext) extends Runnable:
  override def run(): Unit =
    println("Shell Started")
    LazyList
      .continually(())
      .takeWhile(_ => !Thread.interrupted())
      .map(_ => StdIn.readLine("> "))
      .foreach { cmdLine =>
        if cmdLine != "" then
          executeCommandLine(cmdLine, context).left
            .foreach(e => println(e.getMessage))
      }
    println("Shell Stopped")
