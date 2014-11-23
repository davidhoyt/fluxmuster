package com.github.davidhoyt.fluxmuster

/**
 * Provides a way of determining if a provided implicit conversion would be the identity function
 * (effectively a no-op). This is important to prevent the proliferation of unnecessary [[Link]]s.
 *
 * Most of the time this should behave just like a normal implicit conversion and should remain
 * out of the way. The only time it surfaces should be when a function is needed that has the same
 * input and return types but modifies the input in some way. For example:
 *
 * {{{
 *   import Implicitly._
 *
 *   implicit def increment(i: Int): Int = i + 1
 *   implicit def createString(i: Int): String = i.toString
 *
 *   def doWork[A, B](a: A)(implicit f: A ==> B): B = f(a)
 *
 *   doWork[Int, Int](0)                     //Uses identity
 *   doWork[Int, String](0)                  //Uses createString() automatically
 *   doWork[Int, Int](0)(convert(increment)) //Needs to use increment()
 * }}}
 *
 * @tparam T1 The first parameter input type
 * @tparam R The return type
 */
trait ==>[@specialized(Int, Long, Float, Double) -T1, @specialized(Unit, Boolean, Int, Float, Long, Double) +R] extends (T1 => R) {
  val identity: Boolean
  val fn: T1 => R
}

trait LowPriorityConversions {
  import scala.language.implicitConversions

  /**
   * Materializes a new implicit converter which wraps the `given` function and conveys that the
   * identity function is ''not'' in use.
   *
   * @param given A function that converts from `T1` to `R`
   * @tparam T1 The input type
   * @tparam R The return type
   * @return A new implicit converter that wraps the `given` function
   */
  @inline implicit def convert[T1, R](implicit given: T1 => R): T1 ==> R =
    new (T1 ==> R) {
      override val identity = false
      override val fn = given
      override def apply(t: T1): R = given(t)
    }

  /**
   * Materializes a new implicit converter which wraps [[Predef.identity]] and conveys that the identity
   * function is in use. [[Link]] instances should ignore this during sequencing.
   *
   * @tparam T1 The input and return type which are the same
   * @return A new implicit converter that conveys it's identity (a no-op)
   */
  @inline implicit def identity[T1]: T1 ==> T1 =
    new (T1 ==> T1) {
      override val identity = true
      override val fn = Predef.identity[T1] _
      override def apply(t: T1): T1 = t
    }

  /**
   * Converts an implicit converter back into a normal function.
   *
   * @param from The implicit converter to transform
   * @tparam T1 The input type
   * @tparam R The return type
   * @return The original given function for this implicit converter
   */
  @inline implicit def toFunction[T1, R](from: T1 ==> R): T1 => R =
    from.fn
}

object ==> extends LowPriorityConversions
object Implicitly extends LowPriorityConversions
