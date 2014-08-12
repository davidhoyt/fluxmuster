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

trait StepChaining {
  implicit val chain: ChainStep
  def chainTogether(instance: ChainableStep, mine: ChainStep, other: ChainStep): ChainStep
}

object StepProvidedChain {
  import scala.collection.immutable

  def apply(instance: ChainableStep, mine: ChainStep, other: ChainStep): ChainStep =
    if ((mine eq null) || mine.isEmpty)
      immutable.Seq(instance)
    else
      mine
}

object StepCombinedChain {
  def apply(instance: ChainableStep, mine: ChainStep, other: ChainStep): ChainStep =
    (mine ++ other).foldLeft(EmptyChainStep) {
      case (seq, p) if p.chain.nonEmpty =>
        seq :+ p.chain.head
      case (seq, _) =>
        seq
    }
}

