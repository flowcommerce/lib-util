package io.flow.util

trait Shutdownable {
  def isShutdown: Boolean = false
}
