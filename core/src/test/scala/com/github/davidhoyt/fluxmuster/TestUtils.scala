package com.github.davidhoyt.fluxmuster

object TestUtils {
  import scala.annotation.tailrec

  import scala.language.higherKinds
  
  def containsValuesInApproximateOrder[A, F[_], S[_]](first: F[A], second: S[A])(implicit ev1: F[A] <:< Traversable[A], ev2: S[A] <:< Traversable[A]): Boolean = {
    @tailrec def check(firstRemaining: Traversable[A], secondRemaining: Traversable[A]): Boolean = {
      if (secondRemaining.isEmpty)
        true
      else if (firstRemaining.isEmpty)
        false
      else {
        val next = secondRemaining.head
        check(firstRemaining.dropWhile(_ != next).tail, secondRemaining.tail)
      }
    }
    check(ev1(first), ev2(second))
  }
  
  implicit class TraversableEnhancements[A](val t: Traversable[A]) extends AnyVal {
    def containsValuesInApproximateOrder[S[_]](other: S[A])(implicit ev2: S[A] <:< Traversable[A]): Boolean =
      TestUtils.containsValuesInApproximateOrder(t, other)
  }
}
