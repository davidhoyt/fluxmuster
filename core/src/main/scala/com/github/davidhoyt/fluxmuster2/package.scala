package com.github.davidhoyt

import com.twitter.util.NonFatal

package object fluxmuster2 {
  import scala.collection.immutable
  import scala.concurrent._

  type LinkDownstream[-A, +B] = A => B
  type LinkUpstream[-C, +D] = C => D

  type SimpleStep[A, B, C, D] = StepLike[A, B, C, D] with Runner[A, B, C, D] with StepConnections

  type Connections = immutable.Seq[SimpleStep[_, _, _, _]]
  val EmptyConnections = immutable.Seq[SimpleStep[_, _, _, _]]()


  import scala.language.higherKinds
  import scala.language.implicitConversions

  import com.github.davidhoyt.fluxmuster.TypeTagTree

  implicit object FutureLiftOp extends LiftOp[ExecutionContext, Future] {
    def apply[A, D](runner: A => D)(implicit ec: ExecutionContext, connections: Connections, shouldLiftResult: Boolean, typeAccept: TypeTagTree[A], typeResult: TypeTagTree[D]): A => Future[D] =
      (a: A) =>
        future {
          runner(a)
        }

    def point[A](given: => A)(implicit ec: ExecutionContext): Future[A] =
      try Future.successful(given)
      catch {
        case NonFatal(error) =>
          Future.failed(error)
      }

    def map[A, B](given: Future[A])(fn: A => B)(implicit ec: ExecutionContext): Future[B] =
      given.map(fn)(ec)

    def flatten[A](given: Future[Future[A]])(implicit ec: ExecutionContext): Future[A] = {
      import scala.util._
      val p = Promise[A]()
      given.onComplete {
        case Success(next) =>
          p.completeWith(next)
        case Failure(t) =>
          p.failure(t)
      }
      p.future
    }
  }

  implicit object FutureConverter extends (Future -> Future) {
    implicit def apply[A](f: Future[A]): Future[A] = f
  }

  //Courtesy Miles Sabin:
  //  http://www.chuusai.com/2012/05/10/shapeless-polymorphic-function-values-2/

  type Id[T] = T
  type Const[C] = {
    type λ[T] = C
  }

  implicit def polyToMono2[G[_], T](f: Id -> G): T => G[T] = f(_)
  implicit def polyToMono3[F[_], T](f: F -> Id): F[T] => T = f(_)
  implicit def polyToMono4[T](f: Id -> Id) : T => T = f[T](_)
  implicit def polyToMono5[F[_], G, T](f: F -> Const[G]#λ) : F[T] => G = f(_)
  implicit def polyToMono6[G, T](f: Id -> Const[G]#λ) : T => G = f(_)

  implicit object IdentityConverter extends (Id -> Id) {
    implicit def apply[A](a: Id[A]): Id[A] = a
  }

  /**
   * Given a sequence it produces another sequence of tuples that each contain the
   * previous element, the current, and the next element in the sequence.
   *
   * For example:
   * {{{
   *   scala> prevCurrentNext(Seq(0, 1, 2, 3)){case x => x}
   *   res0: Seq[(Int, Int, Int)] = List((None, 0, Some(1)), (Some(0), 1, Some(2)), (Some(1), 2, Some(3)), (Some(2), 3, None))
   * }}}
   *
   * @param xs The sequence to use
   * @param fn A partial function that maps the previous, current, and next elements
   * @tparam T Type of the sequence to use
   * @tparam U Type of the sequence that will be output after mapping through `fn`
   * @return A new sequence after applying `fn` to the previous, current, and next elements
   */
  def prevCurrentNext[T, U](xs: Seq[T])(fn: PartialFunction[(Option[T], T, Option[T]), U]): Seq[U] = {
    def step(prev: Option[T], xs: Seq[T], build: Seq[U]): Seq[U] = {
      val (current, next) = xs match {
        case x +: y +: _ => (Some(x), Some(y))
        case x +: _ => (Some(x), None)
        case _ => (None, None)
      }

      if (xs.nonEmpty) {
        val buildNext =
          if (fn.isDefinedAt(prev, current.get, next))
            build :+ fn(prev, current.get, next)
          else
            build
        step(current, xs.tail, buildNext)
      } else
        build
    }
    step(None, xs, Seq.empty)
  }
}
