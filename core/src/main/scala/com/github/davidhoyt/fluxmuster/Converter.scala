package com.github.davidhoyt.fluxmuster

import scala.annotation.implicitNotFound

import scala.language.higherKinds
import scala.language.implicitConversions

/**
 * An arrow that maps between two categories `From` and `To`.
 *
 * @tparam From The category being mapped from
 * @tparam To The category being mapped to
 */
@implicitNotFound("Unable to find a fluxmuster converter for ${From} -> ${To}")
trait ->[-From[_], +To[_]] {
  implicit def apply[A](a: From[A]): To[A]
}
