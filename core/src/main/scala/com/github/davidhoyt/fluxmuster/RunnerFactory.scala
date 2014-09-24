package com.github.davidhoyt.fluxmuster

import scala.language.higherKinds

trait RunnerFactory[State, Into[_]] {
  import Chains._

  val defaultName: String
  protected val ops: RunnerOps[State, Into]

  def apply[A, B, C, D, S, F[_], G[_]](name: String, runner: Runner[A, B, C, D, S, F, G])(implicit state: State, converter: G -> Into, typeState: TypeTagTree[State], typeGOfD: TypeTagTree[G[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, G, Into] =
    Runner.withRunner(name, runner, state, ops, rewireOnFlatMap = true)

  def apply[A, B, C, D](name: String, proxy: LinkedProxy[A, B, C, D])(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State], typeOut: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, Into, Into] =
    Runner.withUnliftedProxy(name, proxy, RunnerDataChainEmpty, state, ops, rewireOnFlatMap = true)

  def apply[A, D](name: String, link: Link[A, D])(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State], typeOut: TypeTagTree[Into[D]]): Runner[A, A, D, D, State, Into, Into] =
    apply(name, link.toLinkedProxy)

  def apply[A, D](name: String)(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State]): RunnerNeedsProxy[A, D, State, Into] =
    RunnerNeedsProxy(name, state, ops)

  def apply[A, D]()(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State]): RunnerNeedsProxy[A, D, State, Into] =
    RunnerNeedsProxy(defaultName, state, ops)
}
