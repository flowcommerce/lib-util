package io.flow.util

import org.scalatest.Retries.{isRetryable, withRetry}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{Outcome, TestSuite}

trait RetryHelpers extends TestSuite {
  val retryDelay: Span = Span(2, Seconds)

  override def withFixture(test: NoArgTest): Outcome =
    if (isRetryable(test)) // is taggedAs Retryable
      withRetry(retryDelay) { super.withFixture(test) }
    else
      super.withFixture(test)
}
