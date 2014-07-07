package com.github.davidhoyt

import scala.collection._

package object encabulator {
  type Metadata = String
  type Error = Throwable

  type ErrorProcessor = Error => Unit
  type LinkDownstream[-A, +B] = A => B
  type LinkUpstream[-B, +A] = B => A

  type ConnectedMetadata = immutable.Seq[Metadata]

  implicit object ConsoleLogger extends Logger {
    def warn(message: => String) =
      println(s"[WARN] $message")

    def error(message: => String) =
      println(s"[ERROR] $message")
  }

  def errorProcessor(error: Error)(implicit logger: Logger): Unit =
    logger.error(error.getMessage)

  val defaultErrorProcessor = errorProcessor _
}
