package com.github.davidhoyt.fluxmuster

import Chains.{RunnerOpsChain, RunnerOpsChainEmpty, newRunnerOpsChain}

import scala.language.higherKinds

case class ChainedRunnerOps[Into[_]](private val ops: RunnerOpsChain) {
  /** Lifts the provided `value` into context by calling `point()` across all runners in the chain. */
  def point[A](value: A): Into[A] = {
    val f = ops.foldLeft(value: Any) {
      case (a, ops) =>
        ops.point(a)
    }
    f.asInstanceOf[Into[A]]
  }

  def :+(addl: RunnerOpsAny): ChainedRunnerOps[Into] =
    copy(ops = ops :+ addl)
}

object ChainedRunnerOps {
  def apply[Into[_]](): ChainedRunnerOps[Into] =
    ChainedRunnerOps[Into](RunnerOpsChainEmpty)

  def apply[Into[_]](ops: RunnerOpsAny, addl: RunnerOpsAny*): ChainedRunnerOps[Into] =
    ChainedRunnerOps[Into](newRunnerOpsChain(ops) ++ addl)
}