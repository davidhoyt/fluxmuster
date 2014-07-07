package com.github.davidhoyt.encabulator

sealed trait ProxyPass[+A] {
  val ignore: Boolean
  val continue: Boolean = !ignore
  val value: A
  def map[B](fn: A => B): ProxyPass[B] =
    if (ignore)
      ShortCircuit(fn(value))
    else
      MapAndProcess(fn(value))
}

object ProxyPass {
  def unapply[A](proxyPass: ProxyPass[A]): Option[A] =
    proxyPass match {
      case MapAndProcess(value) => Some(value)
      case ShortCircuit(value) => Some(value.asInstanceOf[A])
      case _ => None
    }
}

//Meant to tell downstream portions of the pipeline that they should not modify a value except
sealed case class ShortCircuit[+A](value: A) extends ProxyPass[A] {
  val ignore = true
  override val continue = false
}

sealed case class MapAndProcess[+A](value: A) extends ProxyPass[A] {
  val ignore = false
  override val continue = true
}