package com.github.davidhoyt.fluxmuster

import scala.language.higherKinds

trait RunnerOps[State, Into[_]] {
  import com.typesafe.scalalogging.Logger
  import org.slf4j.LoggerFactory

import scala.language.implicitConversions

  private val logger = Logger(LoggerFactory.getLogger(getClass))

  def liftRunner[A, D](linksChain: ChainLink, opsChain: ChainedRunnerOps[Into], runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Into[D]
  def point[A](given: => A): Into[A]
  def flatten[A](given: Into[Into[A]])(implicit state: State): Into[A]
  def map[A, B](given: Into[A])(fn: A => B)(implicit state: State): Into[B]

  def flatMap[A, B, G[_]](given: Into[A])(fn: A => G[B])(implicit state: State, converter: G -> Into): Into[B] =
    flatten(map(map(given)(fn)(state))(converter.apply)(state))(state)

  def runInThisContext[A, D, From[_]](linksChain: ChainLink, opsChain: ChainedRunnerOps[Into], otherRunner: A => From[D], state: State)(implicit converter: From -> Into, typeIn: TypeTagTree[A], typeFromOut: TypeTagTree[From[D]], typeIntoOut: TypeTagTree[Into[D]]): Link[A, Into[D]] = {
    Link((in: A) => {
      val runOtherInThisContext: A => Into[From[D]] = liftRunner(linksChain, opsChain, otherRunner)(state, typeIn, typeFromOut)
      val resultAfterRunning: Into[From[D]] = runOtherInThisContext(in)

      logger.debug(s"RunnerOps.runInThisContext result after running: $resultAfterRunning")

      val mapResultBackIntoThisContext: Into[Into[D]] = map(resultAfterRunning) { from: From[D] =>
        logger.debug(s"RunnerOps.runInThisContext converting $from [${typeFromOut.toShortString}]")
        val converted: Into[D] = converter.apply(from)
        logger.debug(s"RunnerOps.runInThisContext converted to $converted [${typeIntoOut.toShortString}]")
        //could it be an issue of the map and flatten running async in parallel?
        //no
        converted
      }(state)
      val flattenedBackIntoThisContext: Into[D] = flatten(mapResultBackIntoThisContext)(state)

      flattenedBackIntoThisContext
    })
  }

  /** WARNING: Do not use unless you're sure it's okay. */
  def unsafeCastAsState[S](instance: S): State =
    instance.asInstanceOf[State]

  implicit def asChainableRunnerOps: ChainableRunnerOps =
    this.asInstanceOf[ChainableRunnerOps]
}