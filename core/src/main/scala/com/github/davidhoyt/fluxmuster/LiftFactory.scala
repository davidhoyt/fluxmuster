package com.github.davidhoyt.fluxmuster

import scala.language.higherKinds

trait LiftFactory[State, Into[_]] {
  val defaultName: String
  protected val ops: LiftOps[State, Into]

  def apply[A, D](name: String)(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State]): PartialLift[A, D, State, Into] =
    PartialLift(name, state, ops)

  def apply[A, D]()(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State]): PartialLift[A, D, State, Into] =
    PartialLift(defaultName, state, ops)
}
