package com.github.davidhoyt.fluxmuster5.runner

import com.github.davidhoyt.fluxmuster.TypeTagTree
import com.github.davidhoyt.fluxmuster5._

import scala.language.higherKinds

trait RunnerFactory[State, Into[_]] {
  val defaultName: String
  protected val ops: RunnerOps[State, Into]

  def apply[A, B, C, D, S, F[_], G[_]](name: String, runner: Runner[A, B, C, D, S, F, G])(implicit state: State, converter: G -> Into, typeState: TypeTagTree[State], typeGOfD: TypeTagTree[G[D]], typeIntoOfD: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, G, Into] =
    Runner.withRunner(name, runner, state, ops, rewireOnFlatMap = true)

  def apply[A, B, C, D](name: String, proxy: Proxy[A, B, C, D])(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State], typeOut: TypeTagTree[Into[D]]): Runner[A, B, C, D, State, Into, Into] =
    Runner.withUnliftedProxy(name, proxy, EmptyChainRunner, state, ops, rewireOnFlatMap = true)

  def apply[A, D](name: String, link: Link[A, D])(implicit state: State, converter: Into -> Into, typeState: TypeTagTree[State], typeOut: TypeTagTree[Into[D]]): Runner[A, A, D, D, State, Into, Into] =
    apply(name, link.toProxy)
}
