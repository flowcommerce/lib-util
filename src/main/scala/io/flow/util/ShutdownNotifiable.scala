package io.flow.util

trait ShutdownNotifiable {

  @volatile private var shutdown: Boolean = false

  def shutdownInitiated(): Unit = {
    shutdown = true
  }

  protected def isShutdown: Boolean = shutdown
}

