package com.github.davidhoyt.fluxmuster

object Chains {
  import scala.collection.immutable

  type LinkChain             = immutable.Vector[LinkAny]
  type LiftChain             = immutable.Vector[LiftChainEntryAny]
  type LiftOpsChain          = immutable.Vector[LiftOpsAny]
  type ChainSideEffects[Out] = immutable.Vector[SideEffecting[Out]]

  type FnChainLink =
    (LinkAny, LinkChain, LinkChain) => LinkChain

  val LinkChainEmpty             = immutable.Vector[LinkAny]()
  val LiftChainEmpty             = immutable.Vector[LiftChainEntryAny]()
  val LiftOpsChainEmpty          = immutable.Vector[LiftOpsAny]()
  def SideEffectsChainEmpty[Out] = immutable.Vector[SideEffecting[Out]]()

  def newLinkChain(chainLink: LinkAny*): LinkChain =
    if ((chainLink ne null) && chainLink.nonEmpty)
      immutable.Vector[LinkAny](chainLink:_*)
    else
      LinkChainEmpty

  def newLiftChain(lifts: LiftChainEntryAny*): LiftChain =
    if ((lifts ne null) && lifts.nonEmpty)
      immutable.Vector[LiftChainEntryAny](lifts:_*)
    else
      LiftChainEmpty

  def newLiftOpsChain(ops: LiftOpsAny*): LiftOpsChain =
    if ((ops ne null) && ops.nonEmpty)
      immutable.Vector[LiftOpsAny](ops:_*)
    else
      LiftOpsChainEmpty
}
