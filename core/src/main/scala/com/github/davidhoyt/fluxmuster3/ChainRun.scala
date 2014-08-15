package com.github.davidhoyt.fluxmuster3

/** Specifies work that must have all implicit conversions resolved before running. */
trait ChainRun[In, Out, Next] {
  def runChain(in: In): Out
  def routeChainNext(in: Out): Next
}
