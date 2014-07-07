package com.github.davidhoyt.encabulator

//Encabulator

import scala.collection._
import scala.util._

//PROBLEM WITH THIS SOLUTION
//We may want to optionally short circuit. Therefore we need a context-sensitive approach: MONOIDS?
//Instead perhaps always pass through the pipeline but optionally ignore?
// Ignore(x)/Inspect(x) ?
// Need a way to provide conversion A => B if we should ignore OR inspect (in other words, a map is always required)
//Optionally bounce it back? Should work if we intervene in the Proxy.combine() method and check the return value and if
//it's a Bounce() (Return()?) and the type matches the expected type (B is the same type as D) then send it back
//otherwise throw an exception.
//- Does not work b/c downstream works independent of upstream. Need a way to plug in to upstream
//  directly from any downstream operation.
//  Perhaps work like the State monad? Pass along something in LinkDownstream that can send values back upstream
//  if short circuiting.

trait Logger {
  def warn(message: => String): Unit
  def error(message: => String): Unit
}

trait ProxySpecification[-A, +B, -C, +D] {
  val metadata: ConnectedMetadata
  val downstream: LinkDownstream[A, B]
  val upstream: LinkUpstream[C, D]

  override def toString = s"ProxySpecification(${metadata.mkString(" <~> ")})"
}

object ProxySpecification {
  def apply[A, B, C, D](downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(immutable.Seq("<unknown>"), downstream, upstream)

  def apply[A, B, C, D](metadata: Metadata)(downstream: LinkDownstream[A, B], upstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    apply(immutable.Seq(metadata), downstream, upstream)

  private[encabulator] def apply[A, B, C, D](specMetadata: ConnectedMetadata, specDownstream: LinkDownstream[A, B], specUpstream: LinkUpstream[C, D]): ProxySpecification[A, B, C, D] =
    new ProxySpecification[A, B, C, D] {
      val metadata = specMetadata
      val downstream = specDownstream
      val upstream = specUpstream
    }

  def unapply[A, B, C, D](spec: ProxySpecification[A, B, C, D]): Option[(ConnectedMetadata, LinkDownstream[A, B], LinkUpstream[C, D])] =
    if (spec ne null)
      Some(spec.metadata, spec.downstream, spec.upstream)
    else
      None
}

sealed trait Proxy[-A, +B, -C, +D] extends ProxySpecification[A, B, C, D] {
  lazy val downstream = specification.downstream
  lazy val upstream = specification.upstream

  val metadata: ConnectedMetadata
  val specification: ProxySpecification[A, B, C, D]
  def apply(value: A): Try[D]
}

object Proxy {
  import scala.reflect.runtime.universe._

  def combine[A, B /*: TypeTag*/, C /*: TypeTag*/, E, F, G](p1: ProxySpecification[A, B, G, E], p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] = {
    //val bTag = implicitly[TypeTag[B]]
    //val gTag = implicitly[TypeTag[G]]
    val ProxySpecification(metadata1, downstream1, upstream1) = p1
    val ProxySpecification(metadata2, downstream2, upstream2) = p2

    //val check: LinkDownstream[B, B] = {
    //  case next @ ShortCircuit(_) if gTag.tpe <:< bTag.tpe => upstream1()
    //  case next => next
    //}

    val downstream: LinkDownstream[A, C] = downstream1 andThen downstream2
    val upstream: LinkUpstream[F, E] = upstream1 compose upstream2
    ProxySpecification(metadata1 ++ metadata2, downstream, upstream)
  }

  def identity[X](x: X): X = x

  implicit class ProxySpecificationEnhancements[A, B, G, E](val p1: ProxySpecification[A, B, G, E]) extends AnyVal {
    def <~>[C, F](p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] = combine(p1, p2)
    def connect[C, F](p2: ProxySpecification[B, C, F, G]): ProxySpecification[A, C, F, E] = combine(p1, p2)
  }

  def apply[A, B, C](providedSpec: => ProxySpecification[A, B, B, C]): Proxy[A, B, B, C] =
    new Proxy[A, B, B, C] {
      lazy val specification =
        providedSpec

      lazy val metadata: ConnectedMetadata =
        specification.metadata

      override def toString =
        s"Proxy(${metadata.mkString(" <~> ")})"

      def apply(value: A): scala.util.Try[C] = scala.util.Try {
        //val fromDownstream = specification.downstream.applyOrElse(MapAndProcess(value), { a: ProxyPass[A] => throw new IllegalStateException(s"Unable to process $a") })
        val fromDownstream = specification.downstream(value)
        val result = specification.upstream(fromDownstream)
        result
      }
    }
}

import com.github.davidhoyt.encabulator.Proxy._

object Hystrix {
  def apply(): ProxySpecification[String, Int, Long, Long] =
    ProxySpecification("Hystrix")(Fooz.ps1.downstream, Fooz.ps1.upstream)
}

object Cache {
  def apply(): ProxySpecification[Int, Long, Long, Long] =
    ProxySpecification("Cache")(Fooz.ps2.downstream, Fooz.ps2.upstream)
}

object Passthrough {
  import scala.reflect.runtime.universe._
  val NAME = typeOf[Passthrough.type].termSymbol.asTerm.fullName

  def apply[A, B](onDownstream: A => Unit = {_: A => ()}, onUpstream: B => Unit = {_: B => ()}): ProxySpecification[A, A, B, B] =
    ProxySpecification(NAME)({ a => onDownstream(a); a}, {b => onUpstream(b); b})
}

object Fooz {
  val a1: LinkDownstream[String, Int] = _.toInt
  val a2: LinkDownstream[Int, Long] = _.toLong + 1
  val upstream: LinkUpstream[Long, Long] = Proxy.identity
  val ps1: ProxySpecification[String, Int, Long, Long] = ProxySpecification("to int")(a1, x => x + 10)
  val ps2: ProxySpecification[Int, Long, Long, Long] = ProxySpecification(a2, _ - 1)
  val proxySpec: ProxySpecification[String, Long, Long, Long] = ps1 connect ps2
  val proxy1 = Proxy(proxySpec)
  val proxy = Proxy(Proxy(Passthrough[String, String]()) <~> Proxy(Passthrough[String, String]() <~> Passthrough[String, String]()))
}

//trait Pipe[-A, +B] {
//  def metadata(metadata: Metadata): Metadata = ???
//  def error(error: Error): Unit = ???
//  def map(value: A): B = ???
//  def next[C]: Option[Pipe[]]
//}
//trait Pipe[-A, +B, -C >: A, +D] {
//  def metadata(metadata: Metadata): Metadata = ???
//  def error(error: Error): Unit = ???
//
//  def downstream(value: A): B
//  def upstream(value: C): D
//  def run(value: A): D = {
//    if (next.isEmpty)
//      upstream(value)
//    else
//      next.get.downstream(value)
//    //next map { nxt => nxt.downstream(downstream(value)) }
//  }
//  def next[X, Y]: Option[Pipe[B, X, C, Y]] = None
//  def pipe[X, Y](next: Pipe[B, X, C, Y]): Pipe[A, X, C, Y] = ???
//}

//trait |>[-A, +B] extends Pipe[A, B]

//object R {
//  def identity[A] = (a: A) => a
//
//  def apply[A, B, C, D](downstream: A => B)(upstream: C => D): Pipe[A, B, C, D] =
//    new Pipe[A, B, C, D] {
//
//    }
//}


//object Types {
//  type Metadata = String
//  type MetadataPipe = Metadata => Metadata
//  type PipeValue[A] = Boolean => A
//  type Pipe[-A, +B] = (MetadataPipe, PipeValue[A]) => (Metadata, B)
//}
//
//import Types._
//
//trait Ingest[A, B] {
//  val pipe: Pipe[A, B]
//  def metadata: Metadata
//  def apply(value: A): B
//}
//
//object Pipeline {
//  def apply[A, B](ingestPipe: Pipe[A, B]): Ingest[A, B] =
//    new Ingest[A, B] {
//      val pipe = ingestPipe
//      lazy val metadata = pipe(_ => "META", null)._1
//      def apply(value: A): B = pipe(_ => "", _ => value)._2
//    }
//}
//
//
//
//object Implicits {
//  implicit class PipeEnhancements[A, B](val pipe: Pipe[A, B]) extends AnyVal {
//    def |>[C](next: Pipe[B, C]): Pipe[A, C] =
//      (metadataPipe: MetadataPipe, value: PipeValue[A]) => {
//        val result = pipe(x => metadataPipe(x), value)
//        next(x => metadataPipe(x), _ => result._2)
//      }
//  }
//}
//
//object R {
//  def apply[A, B](map: A => B): Pipe[A, B] =
//    (metadataPipe: MetadataPipe, value: PipeValue[A]) => {
//      (null, map(value(true)))
//    }
//}

object Foo {
//  import Implicits._

//  val f = Pipeline {
//    R[String, Int]{x => println(x); x.toInt} |>
//      R[Int, Double]{x => println(x); x.toDouble} |>
//      R[Double, Long]{x => println(x); x.toLong}
//  }
//  def run = println(f("000123"))

//  val f = R[String, Int, Int, Int]{x => println(x); x.toInt}(R.identity)
//  def run = f.run("START")
  def run() = {
    println(Fooz.proxy1)
    println(Fooz.proxy1("234"))
  }
}

object Main extends App {
  Foo.run()
}