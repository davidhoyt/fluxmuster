package com.github.davidhoyt.fluxmuster3

import scala.annotation.implicitNotFound

@implicitNotFound("Unable to find a fluxmuster converter for ${F} -> ${G}")
trait ->[From[_], To[_]] {
  implicit def apply[A](a: From[A]): To[A]
}

trait Converter[From[_], To[_]] extends (From -> To)
