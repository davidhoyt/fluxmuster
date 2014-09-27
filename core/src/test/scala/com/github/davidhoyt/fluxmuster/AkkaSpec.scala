package com.github.davidhoyt.fluxmuster

import akka.actor.ActorSystem

class AkkaSpec extends UnitSpec {
  import scala.concurrent.Await
  import scala.concurrent.duration._

  import Links._
  import Implicits._
  import Proxies._
  import lift._

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Lift.type]

  it should s"run serially with ${Macros.simpleNameOf[Akka.type]}" in {
    implicit val system = ActorSystem("test")
    import system.dispatcher

    val foo =
      for {
        s <- p1 <~> p2 <~> p3 <~> (((l: Long) => { /*Thread.sleep(3000L);*/ l }, identity[Int] _))
        f <- s |> Akka("", config = AkkaConfig(timeout = 1.second))
      } yield f

    Await.result(foo.run("0"), 10.seconds) should be ("33")

    system.shutdown()
  }
}