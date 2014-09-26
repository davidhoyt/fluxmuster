package com.github.davidhoyt.fluxmuster

object Implicits {
  import scala.language.implicitConversions

  implicit class Tuple2LinkEnhancements[A, B, C, D](val t: (Downstream[A, B], Upstream[C, D])) extends AnyVal {
    implicit def toLinkedProxy(implicit proof: B => C): LinkedProxy[A, B, C, D] =
      toLinkedProxy("<~>")

    implicit def toLinkedProxy(name: String)(implicit proof: B => C): LinkedProxy[A, B, C, D] = {
      val (down, up) = t
      Proxy.linked(name, down, up, proof)
    }

    implicit def toProxy: Proxy[A, B, C, D] =
      toProxy("<~>")

    implicit def toProxy(name: String): Proxy[A, B, C, D] = {
      val (down, up) = t
      Proxy(name, down, up)
    }
  }

  implicit class Tuple2FunctionEnhancements[A, B, C, D](val t: (A => B, C => D)) extends AnyVal {
    implicit def toLinkedProxy(implicit proof: B => C, typeA: TypeTagTree[A], typeB: TypeTagTree[B], typeC: TypeTagTree[C], typeD: TypeTagTree[D]): LinkedProxy[A, B, C, D] =
      toLinkedProxy("<~>")

    implicit def toLinkedProxy(name: String)(implicit proof: B => C, typeA: TypeTagTree[A], typeB: TypeTagTree[B], typeC: TypeTagTree[C], typeD: TypeTagTree[D]): LinkedProxy[A, B, C, D] = {
      val (down, up) = t
      Proxy.linked(name, Link(down), Link(up), proof)
    }

    implicit def toProxy(implicit typeA: TypeTagTree[A], typeB: TypeTagTree[B], typeC: TypeTagTree[C], typeD: TypeTagTree[D]): Proxy[A, B, C, D] =
      toProxy("<~>")

    implicit def toProxy(name: String)(implicit typeA: TypeTagTree[A], typeB: TypeTagTree[B], typeC: TypeTagTree[C], typeD: TypeTagTree[D]): Proxy[A, B, C, D] = {
      val (down, up) = t
      Proxy(name, Link(down), Link(up))
    }
  }

  implicit def tuple2LinkToProxy[A, B, C, D](t: (Downstream[A, B], Upstream[C, D]))(implicit typeA: TypeTagTree[A], typeB: TypeTagTree[B], typeC: TypeTagTree[C], typeD: TypeTagTree[D]) =
    t.toProxy

  implicit def tuple2FunctionToProxy[A, B, C, D](t: (A => B, C => D))(implicit typeA: TypeTagTree[A], typeB: TypeTagTree[B], typeC: TypeTagTree[C], typeD: TypeTagTree[D]) =
    t.toProxy
}
