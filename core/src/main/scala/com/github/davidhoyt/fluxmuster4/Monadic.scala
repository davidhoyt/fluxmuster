package com.github.davidhoyt.fluxmuster4

import scala.language.higherKinds

trait Monadic[S, F[_]] {
  def point[A](given: => A)(implicit state: S): F[A]
  def flatten[A](given: F[F[A]])(implicit state: S): F[A]
  def map[A, B](given: F[A])(fn: A => B)(implicit state: S): F[B]

  def flatMap[A, B, G[_]](given: F[A])(fn: A => G[B])(implicit state: S, converter: G -> F): F[B] =
    flatten(map(map(given)(fn)(state))(converter.apply)(state))(state)
}
