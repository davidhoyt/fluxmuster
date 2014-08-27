package com.github.davidhoyt.fluxmuster4.lift

import com.github.davidhoyt.fluxmuster4._
import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

import scala.concurrent.{ExecutionContext, Future}

trait Hystrix2Config {
  implicit val context: ExecutionContext
}

trait Hystrix2Impl[T] {
  val fallback: Option[T]

  def flatMap[A](given: Chained[A, T])(fn: A => Future[T]): Hystrix2Impl[T] = {
    ???
  }

//  def filter(fn: T => Boolean): Future[T] =
//    ???
//
//  def point[A](given: => A)(implicit state: Hystrix2Config, convert: A => T): Future[T] =
//    FutureLiftOps.point(convert(given))(state.context)
//
//  def flatten[A](given: Future[Future[A]])(implicit state: Hystrix2Config, convert: A => T): Future[T] =
//    FutureLiftOps.flatten(given)(state.context).map(convert)(state.context)
//
//  def map[A, B](given: Future[A])(fn: (A) => B)(implicit state: Hystrix2Config, convert: B => T): Future[T] =
//    FutureLiftOps.map(given)(fn)(state.context).map(convert)(state.context)
//
//  def flatMap[A, B, G[_]](given: Future[A])(fn: A => G[B])(implicit state: Hystrix2Config, converter: G -> Future, convert: B => T): Future[T] =
//    FutureLiftOps.flatten(FutureLiftOps.map(FutureLiftOps.map(given)(fn)(state.context))(converter.apply)(state.context))(state.context).map(convert)(state.context)
//    //flatten(map(map(given)(fn)(state, identity))(converter.apply)(state, identity))(state, identity).map(convert)(state.context)
}

object Hystrix2 {
  def apply[T](fallback: => T) = {
//    import scala.concurrent.ExecutionContext.Implicits.global
//    Future(fallback)
    lazy val f = Some(fallback)
    new Hystrix2Impl[T] {
      lazy val fallback = f
    }
  }
}
