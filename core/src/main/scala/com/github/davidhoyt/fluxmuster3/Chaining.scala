package com.github.davidhoyt.fluxmuster3

trait LinkChaining {
  val chain: ChainLink
  protected def chainTogether(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink
}

object LinkCombinedChain {
  def apply(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
    (mine ++ other).foldLeft(EmptyChainLink) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }
}

object LinkProvidedChain {
  import scala.collection.immutable

  def apply(instance: ChainableLink, mine: ChainLink, other: ChainLink): ChainLink =
    if ((mine eq null) || mine.isEmpty)
      immutable.Seq(instance)
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

