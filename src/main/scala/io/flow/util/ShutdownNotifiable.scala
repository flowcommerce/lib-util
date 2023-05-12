package io.flow.util

trait ShutdownNotifiable {
  def shutdownInitiated(): Unit
}
