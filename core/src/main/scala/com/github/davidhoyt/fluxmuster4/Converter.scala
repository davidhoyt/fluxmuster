package com.github.davidhoyt.fluxmuster4

import scala.annotation.implicitNotFound

import scala.language.higherKinds

@implicitNotFound("Unable to find a fluxmuster converter for ${From} -> ${To}")
trait ->[From[_], To[_]] {
  import scala.language.implicitConversions

  implicit def apply[A](a: From[A]): To[A]
}

trait Converter[From[_], To[_]] extends (From -> To)
