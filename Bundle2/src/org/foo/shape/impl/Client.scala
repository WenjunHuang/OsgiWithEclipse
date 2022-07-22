package org.foo.shape.impl

import org.osgi.framework.{BundleActivator, BundleContext, ServiceReference}

class Client extends BundleActivator:
  override def start(context: BundleContext): Unit =
    println("Client started")


  override def stop(context: BundleContext): Unit =
    println(s"Client stopped")

