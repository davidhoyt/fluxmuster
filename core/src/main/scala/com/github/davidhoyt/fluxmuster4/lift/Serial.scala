package com.github.davidhoyt.fluxmuster4.lift

import com.github.davidhoyt.fluxmuster.TypeTagTree
import com.github.davidhoyt.fluxmuster4._

import scala.util.Try

object Serial {
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

  def apply[A, B, C, D](step: Step[A, B, C, D])(implicit typeOut: TypeTagTree[Try[D]]): Lift[A, D, Unit, Try] =
    Lift.create(step.name, step, EmptyChainLift, (), SerialOps)(typeUnit, step.downstream.typeIn, typeOut)
}
