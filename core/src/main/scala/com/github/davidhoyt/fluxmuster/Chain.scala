package com.github.davidhoyt.fluxmuster

trait Chain[In, Out] {
  import Chains._

  val typeIn: TypeTagTree[In]
  val typeOut: TypeTagTree[Out]

  def asDefaultString: String
  def asShortString: String

  def linkChain: LinkChain
  def runner: In => Out

  def runChain(in: In): Out =
      runChainAny(in).asInstanceOf[Out]

  def runChainAny(in: Any): Any =
    linkChain.runLinkChainAny(in)

  def runAny(in: Any): Any =
    runner(in.asInstanceOf[In])
}
