package com.github.davidhoyt.fluxmuster.runner

import com.github.davidhoyt.fluxmuster._

import scala.util.Try

object Serial extends RunnerFactory[Unit, Try] {
  val defaultName =
    Macros.simpleNameOf[Serial.type]

  protected val ops =
    new RunnerOps[Unit, Try] {
      import com.typesafe.scalalogging.Logger
      import org.slf4j.LoggerFactory
      import Chains._

      private val logger = Logger(LoggerFactory.getLogger(Macros.nameOf[Serial.type] + ".ops"))

      def liftRunner[A, D](linksChain: LinkChain, opsChain: ChainedRunnerOps[Try], runner: A => D)(implicit state: Unit, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Try[D] =
        (a: A) => Try(runner(a))

      def flatten[A](given: Try[Try[A]])(implicit state: Unit): Try[A] = {
        logger.debug(s"Flattening with: $given")
        val r = given.flatten
        logger.debug(s"Result of flattening: $r")
        r
      }

      def point[A](given: => A): Try[A] =
        Try(given)

      def map[A, B](given: Try[A])(fn: A => B)(implicit state: Unit): Try[B] = {
        logger.debug(s"Mapping with: $given")
        val r = given map fn
        logger.debug(s"Result of mapping: $r")
        r
      }
    }
}
