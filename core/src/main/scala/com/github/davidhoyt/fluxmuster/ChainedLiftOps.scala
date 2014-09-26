package com.github.davidhoyt.fluxmuster

import Chains.LiftOpsChain

import scala.language.higherKinds

case class ChainedLiftOps[Into[_]](private val opsChain: LiftOpsChain) {
  /** Lifts the provided `value` into context by calling `point()` across all lift ops in the chain. */
  def point[A](value: A): Into[A] = {
    val f = opsChain.foldLeft(value: Any) {
      case (a, ops) =>
        ops.point(a)
    }
    f.asInstanceOf[Into[A]]
  }

  def :+(addl: LiftOpsAny): ChainedLiftOps[Into] =
    copy(opsChain = opsChain :+ addl)
}

object ChainedLiftOps {
  import Chains._

  def apply[Into[_]](): ChainedLiftOps[Into] =
    ChainedLiftOps[Into](LiftOpsChainEmpty)

  def apply[Into[_]](ops: LiftOpsAny, addl: LiftOpsAny*): ChainedLiftOps[Into] =
    ChainedLiftOps[Into](newLiftOpsChain(ops) ++ addl)
}