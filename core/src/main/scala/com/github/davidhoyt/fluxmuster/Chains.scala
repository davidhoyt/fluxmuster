package com.github.davidhoyt.fluxmuster

object Chains {
  import scala.collection.immutable

  type LinkChain             = immutable.Vector[LinkAny]
  type RunnerDataChain       = immutable.Vector[RunnerDataAny]
  type Runner2Chain          = immutable.Vector[Runner2DataAny]
  type RunnerOpsChain        = immutable.Vector[RunnerOpsAny]
  type ChainSideEffects[Out] = immutable.Vector[SideEffecting[Out]]

  type FnChainLink =
    (LinkAny, LinkChain, LinkChain) => LinkChain

  val LinkChainEmpty             = immutable.Vector[LinkAny]()
  val RunnerDataChainEmpty       = immutable.Vector[RunnerDataAny]()
  val Runner2ChainEmpty          = immutable.Vector[Runner2DataAny]()
  val RunnerOpsChainEmpty        = immutable.Vector[RunnerOpsAny]()
  def EmptyChainSideEffects[Out] = immutable.Vector[SideEffecting[Out]]()

  def newLinkChain(chainLink: LinkAny*): LinkChain =
    if ((chainLink ne null) && chainLink.nonEmpty)
      immutable.Vector[LinkAny](chainLink:_*)
    else
      LinkChainEmpty

  def newRunnerDataChain(runners: RunnerDataAny*): RunnerDataChain =
    if ((runners ne null) && runners.nonEmpty)
      immutable.Vector[RunnerDataAny](runners:_*)
    else
      RunnerDataChainEmpty
  def newRunner2Chain(runners: Runner2DataAny*): Runner2Chain =
    if ((runners ne null) && runners.nonEmpty)
      immutable.Vector[Runner2DataAny](runners:_*)
    else
      Runner2ChainEmpty

  def newRunnerOpsChain(ops: RunnerOpsAny*): RunnerOpsChain =
    if ((ops ne null) && ops.nonEmpty)
      immutable.Vector[RunnerOpsAny](ops:_*)
    else
      RunnerOpsChainEmpty
}
