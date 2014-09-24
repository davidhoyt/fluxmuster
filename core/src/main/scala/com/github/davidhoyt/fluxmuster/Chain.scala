package com.github.davidhoyt.fluxmuster

trait Chain[In, Out] {
  import Chains._

  val typeIn: TypeTagTree[In]
  val typeOut: TypeTagTree[Out]

  def asDefaultString: String
  def asShortString: String

  def chain: LinkChain
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
