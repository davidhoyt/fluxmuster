package com.github.davidhoyt.fluxmuster

import com.github.davidhoyt.fluxmuster.Proxy

import scala.annotation.tailrec

sealed case class Runner2Data[State, From[_], Into[_]](name: String, state: State, ops: RunnerOps[State, Into])(implicit val converter: From -> Into, val typeState: TypeTagTree[State]) {
  def asChainableRunner: ChainableRunner2 =
    this.asInstanceOf[ChainableRunner2]
}

trait Runner2[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut, State, From[_], Into[_]]
extends Run[DownstreamIn, Into[UpstreamOut]]
with Named {

  sealed case class Materialized(state: State, links: ChainLink, runners: ChainRunner2, runner: Link[DownstreamIn, Into[UpstreamOut]])

  def materialize(state: State, ops: RunnerOps[State, Into], links: ChainLink, runners: ChainRunner2): Materialized

  val givenState: State
  val givenLinks: ChainLink
  val givenRunners: ChainRunner2

  val proxy: Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]

  val ops: RunnerOps[State, Into]
  val converter: From -> Into
  val rewireOnFlatMap = false
  val typeState: TypeTagTree[State]
  val typeFrom: TypeTagTree[From[UpstreamOut]]

  lazy val materialized = {
    //Continually iterate until we get back what we put in.
    @tailrec def stabilize(current: Materialized): Materialized = {
      val m @ Materialized(state, links, runners, _) = current
      val next = materialize(state, ops, links, runners)
      if (next == current)
        current
      else
        stabilize(next)
    }
    materialize(givenState, ops, givenLinks, givenRunners)
  }

  def state =
    materialized.state

  def links =
    materialized.links

  def runner =
    materialized.runner

  def runners =
    materialized.runners

  protected def createChainRunnerWithThis(givenState: State, givenRunners: ChainRunner2): ChainRunner2 = {
    val data =
      Runner2Data(name, givenState, ops)(converter, typeState)
        .asChainableRunner

    if ((givenRunners eq null) || givenRunners.isEmpty)
      newChainRunner2(data)
    else
      givenRunners :+ data
  }


}
//  name: String,
//  chain: ChainLink,
//  link: Link[DownstreamIn, Into[UpstreamOut]],
//  originalProxy: Proxy[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut],
//  providedRunnerChain: ChainRunner,
//  providedState: State,
//  ops: RunnerOps[State, Into],
//  converter: From -> Into,
//  rewireOnFlatMap: Boolean = false,
//  mapState: (State, ChainRunner) => State = (s: State, _: ChainRunner) => s,
//  asShortString: String = null
//)(
//  implicit val typeState: TypeTagTree[State],
//  val typeFrom: TypeTagTree[From[UpstreamOut]]
//)
//  extends Run[DownstreamIn, Into[UpstreamOut]]
//  with Named
//{
//}
