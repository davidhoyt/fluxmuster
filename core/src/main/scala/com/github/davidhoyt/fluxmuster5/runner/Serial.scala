package com.github.davidhoyt.fluxmuster5.runner

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}
import com.github.davidhoyt.fluxmuster5._

import scala.util.Try

import scala.language.higherKinds

object Serial {
  val NAME = Macros.simpleNameOf[Serial.type]

  private object SerialOps extends RunnerOps[Unit, Try] {
    def liftRunner[A, D](chained: ChainLink, runner: A => D)(implicit state: Unit, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Try[D] =
      (a: A) => Try(runner(a))

    def flatten[A](given: Try[Try[A]])(implicit state: Unit): Try[A] =
      given.flatten

    def point[A](given: => A)(implicit state: Unit): Try[A] =
      Try(given)

    def map[A, B](given: Try[A])(fn: A => B)(implicit state: Unit): Try[B] =
      given map fn
  }

  def apply[A, B, C, D, S, F[_]](runner: Runner[A, B, C, D, S, F])(implicit converter: F -> Try, typeIntoOfFOfD: TypeTagTree[Try[F[D]]], typeIntoOfD: TypeTagTree[Try[D]]): Runner[A, B, C, D, Unit, Try] = {
    Runner.withRunner(NAME, runner, (), SerialOps, rewireOnFlatMap = true)
  }

  def apply[A, B, C, D](proxy: Proxy[A, B, C, D])(implicit typeOut: TypeTagTree[Try[D]]): Runner[A, B, C, D, Unit, Try] =
    Runner.withUnliftedProxy(NAME, proxy, /*EmptyChainRunner,*/ (), SerialOps, rewireOnFlatMap = true)

  def apply[A, D](link: Link[A, D])(implicit typeOut: TypeTagTree[Try[D]]): Runner[A, A, D, D, Unit, Try] =
    apply(link.toProxy)
}
