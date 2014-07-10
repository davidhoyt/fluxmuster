package foo

//Fluxmuster

import scala.collection._
import scala.util._
import com.github.davidhoyt.fluxmuster._
import Hystrix._

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



import Proxy._

//object Hystrix2 {
//  def apply(): ProxySpecification[String, Int, Long, Long] =
//    ProxySpecification("Hystrix")(Fooz.ps1.downstream, Fooz.ps1.upstream)
//}
//
//object Cache {
//  def apply(): ProxySpecification[Int, Long, Long, Long] =
//    ProxySpecification("Cache")(Fooz.ps2.downstream, Fooz.ps2.upstream)
//}

object Passthrough {
  import scala.reflect.runtime.universe._
  val NAME = typeOf[Passthrough.type].termSymbol.asTerm.fullName

  def apply[A, B](name: String = NAME, onDownstream: A => Unit = {_: A => ()}, onUpstream: B => Unit = {_: B => ()}): ProxySpecification[A, A, B, B] =
    ProxySpecification(name)({ a => onDownstream(a); a}, {b => onUpstream(b); b})
}

object MapBidirectional {
  import scala.reflect.runtime.universe._
  val NAME = typeOf[MapBidirectional.type].termSymbol.asTerm.fullName

  def apply[A, B, C, D](onDownstream: A => B)(onUpstream: C => D): ProxySpecification[A, B, C, D] =
    ProxySpecification(NAME)(onDownstream, onUpstream)
}

object MapDownstream {
  import scala.reflect.runtime.universe._
  val NAME = typeOf[MapDownstream.type].termSymbol.asTerm.fullName

  def apply[A, B, C](onDownstream: A => B): ProxySpecification[A, B, C, C] =
    ProxySpecification(NAME)(onDownstream, identity)
}

object MapUpstream {
  import scala.reflect.runtime.universe._
  val NAME = typeOf[MapUpstream.type].termSymbol.asTerm.fullName

  def apply[A, C, D](onUpstream: C => D): ProxySpecification[A, A, C, D] =
    ProxySpecification(NAME)(identity, onUpstream)
}


object Fooz {

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global


  val hys = Proxy {
    implicit val hystrixConfiguration = HystrixConfiguration("MY-HYSTRIX-GROUP", "MY-HYSTRIX-COMMAND", 2.seconds)
    val LogIt1 = Passthrough[String, Int]("LogIt1", { x => println(s"1: $x"); println(Thread.currentThread.getName)})
    val LogIt2 = MapDownstream[String, Int, Int]({ x =>
      println(s"2: $x");
      println(Thread.currentThread.getName);
      500
    })
    val LogIt3 = MapDownstream[Int, Int, Int]({ x =>
      println(s"3: $x");
      println(Thread.currentThread.getName)
      Thread.sleep(1 * 1000)
      x + 3
    })
    Hystrix(fallback = 100) |> LogIt1 <~> LogIt2 <~> LogIt3 <~> (x => x * 2) <~> (x => x + 1)
  }
  println(hys)
  //val h1: ProxySpecification[String, String, String, String] =
  println(Await.result(hys("MY VALUE"), 10.seconds))
  println(Await.result(hys("MY VALUE 2"), 10.seconds))

  val a1: LinkDownstream[String, Int] = _.toInt
  val a2: LinkDownstream[Int, Long] = _.toLong + 1
  val upstream: LinkUpstream[Long, Long] = identity
  val ps1: ProxySpecification[String, Int, Long, Long] = ProxySpecification("to int")(a1, x => x + 10)
  val ps2: ProxySpecification[Int, Long, Long, Long] = ProxySpecification(a2, _ - 1)
  val proxySpec: ProxySpecification[String, Long, Long, Long] = ps1 connect ps2
  val proxy1 = Proxy(proxySpec)
  //val proxy = Proxy(Proxy(Passthrough[String, String]()) <~> Proxy(Passthrough[String, String]() <~> Passthrough[String, String]()))
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