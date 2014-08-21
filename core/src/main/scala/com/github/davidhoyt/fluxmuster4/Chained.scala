package com.github.davidhoyt.fluxmuster4

import com.github.davidhoyt.fluxmuster.TypeTagTree

trait Chained[In, Out] {
  val typeIn: TypeTagTree[In]
  val typeOut: TypeTagTree[Out]

  def asDefaultString: String
  def asShortString: String

  def chain: ChainLink
  def runner: In => Out

  def runChain(in: In): Out = {
    val ran = chain.foldLeft(in: Any) {
      case (soFar, next) =>
        next.runAny(soFar)
    }
    ran.asInstanceOf[Out]
  }

  def runAny(in: Any): Any =
    runner(in.asInstanceOf[In])
}
