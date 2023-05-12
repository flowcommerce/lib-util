package io.flow.util

trait ShutdownNotified {
  def shutdownInitiated(): Unit
}
