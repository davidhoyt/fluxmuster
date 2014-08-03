package foo

//Fluxmuster

//TODO: Option to Akka-ize a proxy (convert each part to an actor)

import akka.actor.ActorSystem
import akka.util.Timeout

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
//  def apply(): ProxyStep[String, Int, Long, Long] =
//    ProxyStep("Hystrix")(Fooz.ps1.downstream, Fooz.ps1.upstream)
//}
//
//object Cache {
//  def apply(): ProxyStep[Int, Long, Long, Long] =
//    ProxyStep("Cache")(Fooz.ps2.downstream, Fooz.ps2.upstream)
//}


object Fooz {

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val timeout = Timeout(10.seconds)
  implicit val system = ActorSystem("test")

  val hys = Proxy {
    implicit val inMemory = InMemoryCache()
    implicit val hystrixConfiguration = HystrixConfiguration("MY-HYSTRIX-GROUP", "MY-HYSTRIX-COMMAND", 2.seconds)

    val b = Identity[String, Int]("Identity(FIRST)") <~> ((x: String) => x.toLong) <~> Identity[Long, Int]("Identity(SECOND)") <~> (((x: Long) => x, (x: Long) => x.toInt))

    val LogIt1 = SideEffecting[String, Long]("LogIt1", { x: String => ()})
    val LogIt2 = Downstream[String, Int, Long]({ x: String =>
      //println(s"2: $x");
      //println(Thread.currentThread.getName);
      x.toInt
    })
    val LogIt3 = Downstream[Int, Int, Long]({ x: Int =>
      //println(Thread.currentThread.getName + s", 3: $x")
      //Thread.sleep(1 * 1000)
      x * 1
    })

    val step1 = LogIt1 <~> LogIt2 <~> LogIt3 <~> ((x: Int) => { x + 0 }, (y: Long) => { y })
    val step2 = Project.upstreamValue[Int, Int, Long] <~> Cache[Int, Long] <~> KeyValueProcessor[Int, Int, Long] { k: Int => /*println(s"IN KVProcessor: $k");*/ k + 1L }
    val step3 = step1 <~> step2

    //problem is that lifting is flattening! :/

    val hystrixize: ProxyStep[String, Future[Long], Future[Long], Future[Long]] =
      Hystrix(0L) |> step3

    val akkaize = //: ProxyStep[String, Future[Long], Future[Long], Future[Long]] =
      Akka.par(AkkaConfiguration()) blah hystrixize

      //(Akka.par(AkkaConfiguration()) |> (Hystrix(0L) |> step3))

    //val hystrixize = Hystrix(0L) |> step3
//    val z = Projection.upstream[Int, Int, Int] <~> Cache[Int, Int](inMemory) <~> KeyValueProcessor[Int, Int, Int] { k => println(s"IN KVProcessor: $k"); k + 1 }
//    val o = Identity[Int, String] <~> ((x: Int) => x + 0, (y: Long) => y.toString) <~> Identity[Int, Long] <~> Projection.upstreamTuple2[Int, Int, Long] <~> Cache[Int, Long](inMemory) <~> KeyValueProcessor[Int, Int, Long] { k => println(s"IN KVProcessor: $k"); k + 1L }
//    val fa: ProxyStep[String, Future[Long], Future[Long], Future[Long]] =
//      Akka(AkkaConfiguration()) |> (LogIt1 <~> LogIt2 <~> LogIt3 <~> ((x: Int) => x + 0, (y: Long) => y) <~> Projection.upstreamTuple2[Int, Int, Long] <~> Cache[Int, Long](inMemory) <~> KeyValueProcessor[Int, Int, Long] { k => println(s"IN KVProcessor: $k"); k + 1L })
//    val fb: ProxyStep[String, Future[Future[Long]], Future[Future[Long]], Future[Future[Long]]] = (Hystrix(fallback = Future.successful(100L)) |> fa)
//    val f: ProxyStep[String, Future[Future[Long]], Future[Future[Long]], Future[Long]] = Join("") <~> fb
//    f
    akkaize
  }
  println(hys)
  println(hys.connections)
  println(Ehcache.availableCaches)
  val sequence = Future.sequence(
    for(i <- Stream.from(0).take(1))
      //val r = Await.result(hys(s"${i % 250}"), 2.seconds)
      //println(s"$i: $r")
      yield hys(s"${i % 50}") map { x =>
        println(s"$i: $x")
      }
      //if (i % 50 == 0)
      //  Thread.sleep(1)
    )
  Await.result(sequence, 10.seconds)
  println("ALL DONE")
  system.shutdown()
//  //val h1: ProxyStep[String, String, String, String] =
//  for (i <- Stream.from(0).take(1).par) {
//    println(s"$i")
//    try {
//      hys.apply(s"VALUE $i")
//    } catch {
//      case t: Throwable => println(t)
//    }
//    //println(Await.result(hys(s"VALUE $i"), 10.seconds))
//  }
//  Thread.sleep(10000)
//  //println(Await.result(hys("MY VALUE 2"), 10.seconds))
//
//  val a1: LinkDownstream[String, Int] = _.toInt
//  val a2: LinkDownstream[Int, Long] = _.toLong + 1
//  val upstream: LinkUpstream[Long, Long] = identity
//  val ps1: ProxyStep[String, Int, Long, Long] = ProxyStep("to int")(a1, x => x + 10)
//  val ps2: ProxyStep[Int, Long, Long, Long] = ProxyStep(a2, _ - 1)
//  val proxyStep: ProxyStep[String, Long, Long, Long] = ps1 connect ps2
//  val proxy1 = Proxy(proxyStep)
//  //val proxy = Proxy(Proxy(Passthrough[String, String]()) <~> Proxy(Passthrough[String, String]() <~> Passthrough[String, String]()))
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
    println(Fooz.hys)
//    println(Fooz.proxy1)
//    println(Fooz.proxy1("234"))
  }
}

object Main extends App {
  Foo.run()
}