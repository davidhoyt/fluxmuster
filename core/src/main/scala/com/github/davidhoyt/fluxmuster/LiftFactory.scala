package com.github.davidhoyt.fluxmuster

import scala.language.higherKinds

trait LiftFactory[State, Into[_]] {
  import Chains._

  val defaultName: String
  protected val ops: LiftOps[State, Into]

  def apply[A, B, C, D, S, F[_], G[_]](name: String, lift: Lift[A, B, C, D, S, F, G])(implicit state: State, converter: G -> Into, typeState: TypeTagTree[State], typeGOfD: TypeTagTree[G[D]], typeIntoOfD: TypeTagTree[Into[D]]): Lift[A, B, C, D, State, G, Into] =
    Lift.lift(name, lift, state, ops, rewireOnFlatMap = true)

  def apply[A, B, C, D](name: String, proxy: LinkedProxy[A, B, C, D])(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State], typeOut: TypeTagTree[Into[D]]): Lift[A, B, C, D, State, Into, Into] =
    Lift.proxy(name, proxy, LiftChainEmpty, state, ops, rewireOnFlatMap = true)

  def apply[A, D](name: String, link: Link[A, D])(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State], typeOut: TypeTagTree[Into[D]]): Lift[A, A, D, D, State, Into, Into] =
    apply(name, link.toLinkedProxy)

  def apply[A, D](name: String)(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State]): PartialLift[A, D, State, Into] =
    PartialLift(name, state, ops)

  def apply[A, D]()(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State]): PartialLift[A, D, State, Into] =
    PartialLift(defaultName, state, ops)
}
