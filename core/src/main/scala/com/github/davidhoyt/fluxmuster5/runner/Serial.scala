package com.github.davidhoyt.fluxmuster5.runner

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}
import com.github.davidhoyt.fluxmuster5._

import scala.util.Try

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

  def apply[A, D](link: Link[A, D])(implicit typeOut: TypeTagTree[Try[D]]): Runner[A, D, Unit, Try] =
    Runner.withUnliftedLink(NAME, link, EmptyChainRunner, (), SerialOps, rewireOnFlatMap = true)(typeUnit, link.typeIn, typeOut)

  def apply[A, B, C, D](proxy: ProxyNeedsProof[A, B, C, D])(implicit proof: B => C, typeOut: TypeTagTree[Try[D]]): Runner[A, D, Unit, Try] =
    apply(Proxy(proxy.name, proxy.downstream, proxy.upstream, proof)(proxy.downstream.typeOut, proxy.upstream.typeIn).toLink)
}
