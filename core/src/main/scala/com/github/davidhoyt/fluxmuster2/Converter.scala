package com.github.davidhoyt.fluxmuster2

import scala.annotation.implicitNotFound

import scala.language.higherKinds
import scala.language.implicitConversions

@implicitNotFound("Unable to find a fluxmuster converter for ${F} -> ${G}")
trait ->[F[_], G[_]] {
  implicit def apply[A](a: F[A]): G[A]
}

trait Converter[F[_], G[_]] extends (F -> G)
