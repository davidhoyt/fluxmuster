package com.github.davidhoyt.fluxmuster3

import scala.collection._

trait ChainingStrategy {
  val chain: ChainLink
  protected def chainTogether(mine: ChainLink, other: ChainLink): ChainLink
}

trait CombinedChain extends ChainingStrategy {
  protected def chainTogether(mine: ChainLink, other: ChainLink): ChainLink =
    (mine ++ other).foldLeft(EmptyChainLink) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }
}

trait ProvidedChain extends ChainingStrategy { self: Link =>
  protected def chainTogether(mine: ChainLink, other: ChainLink): ChainLink =
    if ((mine eq null) || mine.isEmpty)
      immutable.Seq(self: Link)
    else
      mine
}