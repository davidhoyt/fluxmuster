package com.github.davidhoyt.fluxmuster3

/** Specifies work that must have all implicit conversions resolved before running. */
trait ChainRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  def routeDownToUp(in: DownstreamOut): UpstreamIn
  def runUpChain(in: UpstreamIn): UpstreamOut
  def runDownChain(in: DownstreamIn): DownstreamOut

  def runChain(in: DownstreamIn): UpstreamOut =
    runUpChain(routeDownToUp(runDownChain(in)))
}
