package com.github.davidhoyt

import scala.collection._

package object fluxmuster {
  //type Meta = Metadata
  type Error = Throwable

  type ErrorProcessor = Error => Unit

  type LinkDownstream[-TAcceptDownstream, +TMappedDownstream] = TAcceptDownstream => TMappedDownstream
  type LinkUpstream[-TAcceptUpstream, +TMappedUpstream] = TAcceptUpstream => TMappedUpstream

  type ConnectedMetadata = immutable.Seq[Metadata]

  def identity[X](x: X): X = x

  /**
   *
   * @tparam A Expected accepted downstream type of connected step
   * @tparam B Expected downstream type of connected step that will be mapped from `A`
   * @tparam C Expected upstream type of connected step that will be provided after
   *           downstream processing is complete and is coming back upstream
   * @tparam D Expected upstream type of connected step that will be mapped from `C`
   *           when the value is coming back upstream
   * @tparam E Type that is accepted for processing downstream
   * @tparam F Type that will be mapped to downstream given type `A`
   * @tparam G Type that will be provided after downstream processing is complete and
   *           is coming back upstream
   * @tparam H Type that will be mapped to upstream given type `C`
   */
  type ProxyLift[A, B, C, D, E, F, G, H] =
    ProxyStep[A, B, C, D] => ProxyStep[E, F, G, H]

  import scala.language.higherKinds
  import scala.language.implicitConversions

//  implicit def toProxyLift[A, B, C, D, E, F, G, H](fn: FnProxyLift[A, B, C, D, E, F, G, H]): ProxyLift[A, B, C, D, E, F, G, H] =
//    new ProxyLift[A, B, C, D, E, F, G, H] {
//      def apply(p: ProxyStep[A, B, C, D]) =
//        fn(p)
//    }

  implicit class ProxyLiftEnhancements[A, B, C, D, E, F, G, H](val proxy: ProxyLift[A, B, C, D, E, F, G, H]) extends AnyVal {
    def |>(p2: ProxyStep[A, B, C, D]): ProxyStep[E, F, G, H] =
      lift(p2)

    def lift(p2: ProxyStep[A, B, C, D]): ProxyStep[E, F, G, H] =
      proxy(p2)
  }

  import scala.concurrent._

  implicit def toFuture[A](a: A): Future[A] =
    Future.successful(a)

  implicit def flattenFuture[A](f: Future[Future[A]])(implicit ec: ExecutionContext): Future[A] = {
    import scala.util._
    val p = Promise[A]()
    f.onComplete {
      case Success(next) =>
        p.completeWith(next)
      case Failure(t) =>
        p.failure(t)
    }
    p.future
  }

//  trait Foo2[F[_]] {
//    def test[A, B, C](step: ProxyStep[A, B, B, C])(implicit flatten: F[F[C]] => F[C])
//    def |>[A, C](step: ProxyStep[A, F[C], F[C], F[C]])(implicit flatten: F[F[C]] => F[C]): ProxyStep[A, F[C], F[C], F[C]] = {
//      step.
//    }
//  }

  /**
   * Intended for use by [[ProxyLift]] implementers to aid the compiler in
   * capturing type information that may not be normally easily accessible
   * when using the pure functional approach. This allows type inference to
   * be deferred to when the [[ProxyLift]] is applied to a [[ProxyStep]]
   * instance.
   *
   * @tparam F A type constructor that implementers will lift types into
   */
  trait ProxyLiftDownstreamWithHint[T, F[_]] {
    def name: String
    def downstream[A, B, C](step: ProxyStep[A, B, B, C])(implicit convert: T => C, tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): LinkDownstream[A, F[C]]
    def downstream2[A, C](step: ProxyStep[A, F[C], F[C], F[C]])(implicit convert: T => F[C]): LinkDownstream[A, F[F[C]]] =
      ???

    //def |>[A, C](step: ProxyStep[A, F[C], F[C], F[C]])(implicit point: T => F[C], flatten: F[F[C]] => F[C], tA: TypeTagTree[A], tFofC: TypeTagTree[F[C]]): ProxyStep[A, F[C], F[C], F[C]] =
    //  lift(step)(point, flatten, tA, tFofC)

    def blah[A, C](step: ProxyStep[A, F[C], F[C], F[C]])(implicit flatten: F[F[C]] => F[C], evidence: T <:< F[C], tA: TypeTagTree[A], tC: TypeTagTree[C], tFofC: TypeTagTree[F[C]], tFofFofC: TypeTagTree[F[F[C]]]) = {//: ProxyStep[A, F[C], F[C], F[C]] = {
////      val liftedName = name
////
////      val down1: LinkDownstream[T, F[C]] = (t: T) => point(huh(t))
////      val up1: LinkUpstream[A, F[]
////      val fn = (t: T) => huh(t)
////      val
////
////
//      val liftedName = name
//      val liftedDownstream: LinkDownstream[A, F[F[C]]] = downstream2(step)
//      val liftedUpstream: LinkUpstream[F[F[C]], F[C]] = flatten
////      //val foo = (a: A) => { flatten(liftedDownstream(a)) }
////      val liftedUpstream: LinkUpstream[F[F[C]], F[C]] = flatten // identity // flatten //identity[F[F[C]]]
////      val s = ProxyStep(Metadata("FOO", tA, tA, tA, tA, lifted = false, asString = s"???") +: step.metadata, liftedDownstream, liftedUpstream, step.connections)
//      ProxyStep(Metadata(liftedName, tA, tFofC, tFofC, tFofC, lifted = true, asString = s"$liftedName[${tA.toShortString}, ${tFofC.toShortString}]") +: step.metadata, liftedDownstream, liftedUpstream, immutable.Seq(step))
      val liftedName = name
      //val f: ProxyStep[A, F[F[C]], F[F[C]], F[F[C]]] = lift[A, F[C], F[C]](step)
      val s: ProxyStep[A, F[C], F[C], F[C]] = ProxyStep(immutable.Seq(Metadata(liftedName, tA, tFofC, tFofC, tFofC, metadata = step.metadata)), downstream(step) andThen flatten, identity, immutable.Seq(step))
      //val c: ProxyStep[A, F[F[C]], F[F[C]], F[C]] = ProxyStep.combine(f, Upstream[A, F[F[C]], F[C]](flatten))
      s
    }

    def |>[A, B, C](step: ProxyStep[A, B, B, C])(implicit evidence: T <:< C, tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tFofC: TypeTagTree[F[C]]): ProxyStep[A, F[C], F[C], F[C]] =
      lift(step)

    def lift[A, B, C](step: ProxyStep[A, B, B, C])(implicit evidence: T <:< C, tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tFofC: TypeTagTree[F[C]]): ProxyStep[A, F[C], F[C], F[C]] = {
      val liftedName = name
      val liftedUpstream = identity[F[C]] _
      val liftedDownstream = downstream(step)

      ProxyStep(immutable.Seq(Metadata(liftedName, tA, tC, tC, tC, metadata = step.metadata, lifted = true, asString = s"$liftedName[${tA.toShortString}, ${tC.toShortString}]")), liftedDownstream, liftedUpstream, immutable.Seq(step))
    }
  }

  implicit def functionToDownstreamProxyStep[A, B, C](fn: A => B)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C]): ProxyStep[A, B, C, C] =
    Downstream[A, B, C](fn)(tA, tB, tC)

  implicit class Function1ConnectEnhancements[A, B](val fn: A => B) extends AnyVal {
    def <~>[C, D, E](p2: ProxyStep[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D], tE: TypeTagTree[E]): ProxyStep[A, C, D, E] =
      connect(p2)(tA, tB, tC, tD, tE)

    def connect[C, D, E](p2: ProxyStep[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D], tE: TypeTagTree[E]): ProxyStep[A, C, D, E] =
      Downstream[A, B, E](fn)(tA, tB, tE) connect p2
  }

  implicit class Function1BiDirectionalEnhancements[A, B](val fn: A => B) extends AnyVal {
    def <~[C, D](upstreamNext: LinkUpstream[C, A])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[D, D, C, B] =
      upstream(upstreamNext)(tA, tB, tC, tD)

    def upstream[C, D](upstreamNext: LinkUpstream[C, A])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[D, D, C, B] =
      Upstream[D, A, B](fn)(tD, tA, tB) connect Upstream[D, C, A](upstreamNext)(tD, tC, tA)

    def ~>[C, D](next: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, C, D, D] =
      downstream(next)(tA, tB, tC, tD)

    def downstream[C, D](next: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, C, D, D] =
      Downstream[A, B, D](fn)(tA, tB, tD) connect Downstream[B, C, D](next)(tB, tC, tD)
  }

  implicit def tuple2Function1ToProxyStep[A, B, C, D](t: (A => B, C => D))(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): ProxyStep[A, B, C, D] =
    FnTuple2(t)(tA, tB, tC, tD)

  implicit class Tuple2Function1Enhancements[A, B, E, F](val t: (A => B, E => F)) extends AnyVal {
    def <~>[C, D](p2: ProxyStep[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tE: TypeTagTree[E], tF: TypeTagTree[F]): ProxyStep[A, C, D, F] =
      connect(p2)(tA, tB, tE, tF)

    def connect[C, D](p2: ProxyStep[B, C, D, E])(implicit tA: TypeTagTree[A], tB: TypeTagTree[B], tE: TypeTagTree[E], tF: TypeTagTree[F]): ProxyStep[A, C, D, F] =
      FnTuple2(t)(tA, tB, tE, tF) <~> p2
  }

  implicit def functionToProxyStep[A, B, C, D](fn: A => B)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): ProxyStep[A, B, B, B] =
    ProxyStep(immutable.Seq(Metadata(fn.toString(), tA, tB, tB, tB)), fn, identity)

  implicit class ProxyStepConnectEnhancements[A, B, G, E](val p1: ProxyStep[A, B, G, E]) extends AnyVal {
    def <~>[C, F](p2: ProxyStep[B, C, F, G]): ProxyStep[A, C, F, E] =
      connect(p2)

    def connect[C, F](p2: ProxyStep[B, C, F, G]): ProxyStep[A, C, F, E] =
      ProxyStep.combine(p1, p2)

    def <~>[C, F](link: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tG: TypeTagTree[G], tE: TypeTagTree[E]): ProxyStep[A, C, G, E] =
      connect(link)(tA, tC, tG, tE)

    def connect[C, F](link: LinkDownstream[B, C])(implicit tA: TypeTagTree[A], tC: TypeTagTree[C], tG: TypeTagTree[G], tE: TypeTagTree[E]): ProxyStep[A, C, G, E] = {
      val step = ProxyStep(immutable.Seq(Metadata(link.toString(), tA, tC, tG, tE)), link, identity[G])
      val combined = ProxyStep.combine(p1, step)
      combined
    }
  }

  implicit class ProxyStepBiDirectionalEnhancements[A, B, C, D](val step: ProxyStep[A, B, C, D]) extends AnyVal {
    def <~[E](onUpstream: LinkUpstream[E, C])(implicit tB: TypeTagTree[B], tC: TypeTagTree[C], tE: TypeTagTree[E]): ProxyStep[A, B, E, D] =
      step connect Upstream[B, E, C](onUpstream)(tB, tE, tC)

    def ~>[E](onDownstream: LinkDownstream[B, E])(implicit tB: TypeTagTree[B], tC: TypeTagTree[C], tE: TypeTagTree[E]): ProxyStep[A, E, C, D] =
      step connect Downstream[B, E, C](onDownstream)(tB, tE, tC)
  }

  implicit class ProxyStepLiftEnhancements[A, C, F[_]](val step: ProxyStep[A, F[C], F[C], F[C]]) extends AnyVal {
    def |>[T](liftWithHint: ProxyLiftDownstreamWithHint[T, F])(implicit point: T => F[C], flatten: F[F[C]] => F[C], tA: TypeTagTree[A], tFofFofC: TypeTagTree[F[F[C]]], tFofC: TypeTagTree[F[C]]): ProxyStep[A, F[F[C]], F[F[C]], F[C]] =
      lift(liftWithHint)(point, flatten, tA, tFofFofC, tFofC)

    def lift[T](liftWithHint: ProxyLiftDownstreamWithHint[T, F])(implicit point: T => F[C], flatten: F[F[C]] => F[C], tA: TypeTagTree[A], tFofFofC: TypeTagTree[F[F[C]]], tFofC: TypeTagTree[F[C]]): ProxyStep[A, F[F[C]], F[F[C]], F[C]] = {
      val liftedName = liftWithHint.name
      val liftedDownstream: LinkDownstream[A, F[F[C]]] = liftWithHint.downstream(step)(point, tA, tFofC, tFofC)
      val liftedUpstream: LinkUpstream[F[F[C]], F[C]] = flatten
      ProxyStep(immutable.Seq(Metadata(liftedName, tA, tFofFofC, tFofFofC, tFofC, metadata = step.metadata, lifted = true, asString = s"$liftedName[${tA.toShortString}, ${tFofC.toShortString}]")), liftedDownstream, liftedUpstream, immutable.Seq(step))
    }
  }

  implicit class ConnectedMetadataEnhancements(val connectedMetadata: ConnectedMetadata) extends AnyVal {
    def toShortString = {
      val sb = StringBuilder.newBuilder
      var last: Metadata = null
      for ((meta, idx) <- connectedMetadata.zipWithIndex) {
        if (idx > 0) {
          if (meta.lifted || last.lifted)
            sb ++= " |> "
          else
            sb ++= " <~> "
        }
        sb ++= meta.toShortString
        last = meta
      }
      sb.toString()
    }
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

  trait Logger {
    def warn(message: => String): Unit
    def error(message: => String): Unit
  }

  implicit object ConsoleLogger extends Logger {
    def warn(message: => String) =
      println(s"[WARN] $message")

    def error(message: => String) =
      println(s"[ERROR] $message")
  }

//  implicit val DefaultHystrixConfiguration =
//    HystrixConfiguration("<default>", "<unknown>")

  def errorProcessor(error: Error)(implicit logger: Logger): Unit =
    logger.error(error.getMessage)

  val defaultErrorProcessor = errorProcessor _
}
