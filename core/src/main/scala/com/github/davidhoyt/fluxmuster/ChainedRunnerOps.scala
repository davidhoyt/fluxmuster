package com.github.davidhoyt.fluxmuster

import scala.language.higherKinds

case class ChainedRunnerOps[Into[_]](private val ops: ChainRunnerOps) {
  /** Lifts the provided `value` into context by calling `point()` across all runners in the chain. */
  def point[A](value: A): Into[A] = {
    val f = ops.foldLeft(value: Any) {
      case (a, ops) =>
        ops.point(a)
    }
    f.asInstanceOf[Into[A]]
  }

  def :+(addl: ChainableRunnerOps): ChainedRunnerOps[Into] =
    copy(ops = ops :+ addl)
}

object ChainedRunnerOps {
  def apply[Into[_]](): ChainedRunnerOps[Into] =
    ChainedRunnerOps[Into](EmptyChainRunnerOps)

  def apply[Into[_]](ops: ChainableRunnerOps, addl: ChainableRunnerOps*): ChainedRunnerOps[Into] =
    ChainedRunnerOps[Into](newChainRunnerOps(ops) ++ addl)
}