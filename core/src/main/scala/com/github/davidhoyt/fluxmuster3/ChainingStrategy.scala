package com.github.davidhoyt.fluxmuster3

import scala.collection._

trait LinkChainingStrategy {
  val chain: ChainLink
  protected def chainTogether(mine: ChainLink, other: ChainLink): ChainLink
}

trait LinkCombinedChain extends LinkChainingStrategy {
  protected def chainTogether(mine: ChainLink, other: ChainLink): ChainLink =
    (mine ++ other).foldLeft(EmptyChainLink) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }
}

trait LinkProvidedChain extends LinkChainingStrategy { self: ChainableLink =>
  protected def chainTogether(mine: ChainLink, other: ChainLink): ChainLink =
    if ((mine eq null) || mine.isEmpty)
      immutable.Seq(self)
    else
      mine
}

trait BiDirectionalChaining {
  implicit val chain: ChainBiDi
  def chainTogether(instance: ChainableBiDi, mine: ChainBiDi, other: ChainBiDi): ChainBiDi
}

object BiDirectionalProvidedChain {
  import scala.collection.immutable

  def apply(instance: ChainableBiDi, mine: ChainBiDi, other: ChainBiDi): ChainBiDi =
    if ((mine eq null) || mine.isEmpty)
      immutable.Seq(instance)
    else
      mine
}

object BiDirectionalCombinedChain {
  def apply(instance: ChainableBiDi, mine: ChainBiDi, other: ChainBiDi): ChainBiDi =
    (mine ++ other).foldLeft(EmptyChainBiDi) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }
}

