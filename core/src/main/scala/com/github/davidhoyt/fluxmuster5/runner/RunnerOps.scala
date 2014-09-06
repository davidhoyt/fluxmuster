package com.github.davidhoyt.fluxmuster5.runner

import com.github.davidhoyt.fluxmuster.TypeTagTree
import com.github.davidhoyt.fluxmuster5._

import scala.language.higherKinds

trait RunnerOps[State, Into[_]] {
  import scala.language.implicitConversions

  def liftRunner[A, D](chained: ChainLink, runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Into[D]
  def point[A](given: => A)(implicit state: State): Into[A]
  def flatten[A](given: Into[Into[A]])(implicit state: State): Into[A]
  def map[A, B](given: Into[A])(fn: A => B)(implicit state: State): Into[B]

  def flatMap[A, B, G[_]](given: Into[A])(fn: A => G[B])(implicit state: State, converter: G -> Into): Into[B] =
    flatten(map(map(given)(fn)(state))(converter.apply)(state))(state)

  def runInThisContext[A, D, From[_]](chain: ChainLink, otherRunner: A => From[D], state: State)(implicit converter: From -> Into, typeIn: TypeTagTree[A], typeFromOut: TypeTagTree[From[D]], typeIntoOut: TypeTagTree[Into[D]]): Link[A, Into[D]] = {
    Link((in: A) => {
      val runOtherInThisContext: A => Into[From[D]] = liftRunner(chain, otherRunner)(state, typeIn, typeFromOut)
      val resultAfterRunning: Into[From[D]] = runOtherInThisContext(in)

      val mapResultBackIntoThisContext = map(resultAfterRunning)(converter.apply)(state)
      val flattenedBackIntoThisContext: Into[D] = flatten(mapResultBackIntoThisContext)(state)

      flattenedBackIntoThisContext
    })
  }

  /** WARNING: Do not use unless you're sure it's okay. */
  def unsafeCastAsState[S](instance: S): State =
    instance.asInstanceOf[State]
}