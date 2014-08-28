package com.github.davidhoyt.fluxmuster4.lift

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}
import com.github.davidhoyt.fluxmuster4._

import scala.util.Try

object Serial {
  val NAME = Macros.simpleNameOf[Serial.type]

  private object SerialOps extends LiftOps[Unit, Try] {
    def liftRunner[A, D](chained: ChainLink, runner: A => D)(implicit state: Unit, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Try[D] =
      (a: A) => Try(runner(a))

    def flatten[A](given: Try[Try[A]])(implicit state: Unit): Try[A] =
      given.flatten

    def point[A](given: => A)(implicit state: Unit): Try[A] =
      Try(given)

    def map[A, B](given: Try[A])(fn: A => B)(implicit state: Unit): Try[B] =
      given map fn
  }

  def apply[A, D](chained: Chained[A, D])(implicit typeOut: TypeTagTree[Try[D]]): Lift[A, D, Unit, Try] =
    Lift.withChained(NAME, chained, EmptyChainLift, (), SerialOps)(typeUnit, chained.typeIn, typeOut)
}
