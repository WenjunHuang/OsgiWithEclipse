package org.foo.shape

import org.osgi.framework.{BundleActivator, BundleContext}

class Activator extends BundleActivator:
  override def start(context: BundleContext): Unit =
    println("Bundle started")

  override def stop(context: BundleContext): Unit =
    println("Bundle stopped")

