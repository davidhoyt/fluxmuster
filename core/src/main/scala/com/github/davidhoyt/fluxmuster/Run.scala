package com.github.davidhoyt.fluxmuster

trait Run[In, Out] {
  def apply[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
    outToB(run(aToIn(a)))

  def run(in: In): Out
}
