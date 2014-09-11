package com.github.davidhoyt.fluxmuster5.runner

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}
import com.github.davidhoyt.fluxmuster5._

import scala.util.Try

object Serial extends RunnerFactory[Unit, Try] {
  val defaultName =
    Macros.simpleNameOf[Serial.type]

  protected val ops =
    new RunnerOps[Unit, Try] {
      def liftRunner[A, D](chained: ChainLink, runner: A => D)(implicit state: Unit, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Try[D] =
        (a: A) => Try(runner(a))

      def flatten[A](given: Try[Try[A]])(implicit state: Unit): Try[A] = {
        println(s"SerialOps.flatten: $given")
        val r = given.flatten
        println(s"SerialOps.flatten.result: $r")
        r
      }

      def point[A](given: => A)(implicit state: Unit): Try[A] =
        Try(given)

      def map[A, B](given: Try[A])(fn: A => B)(implicit state: Unit): Try[B] = {
        println(s"SerialOps.map: $given")
        val r = given map fn
        println(s"SerialOps.map.result: $r")
        r
      }
    }
}
