package com.github.davidhoyt.fluxmuster

object Implicits {
  import scala.language.implicitConversions

  implicit def functionToLink[In, Out](fn: In => Out)(implicit in: TypeTagTree[In], out: TypeTagTree[Out]): Link[In, Out] =
    fn.toLink

  implicit def functionToLink[In, Out](name: String)(fn: In => Out)(implicit in: TypeTagTree[In], out: TypeTagTree[Out]): Link[In, Out] =
    fn.toLink(name)

  implicit class FunctionEnhancements[In, Out](val fn: In => Out) extends AnyVal {
    def toLink(implicit linkIn: TypeTagTree[In], linkOut: TypeTagTree[Out]): Link[In, Out] =
      toLink(s"(${linkIn.toShortString} => ${linkOut.toShortString})")

    def toLink(name: String)(implicit linkIn: TypeTagTree[In], linkOut: TypeTagTree[Out]): Link[In, Out] = {
      val linkName = name
      new Link[In, Out] {
        override val in = linkIn
        override val out = linkOut
        override val name = linkName
        override protected def process[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B =
          outToB(fn(aToIn(a)))
      }
    }
  }
}
