package com.github.davidhoyt.fluxmuster

import scala.concurrent.Await

class HystrixSpec extends UnitSpec {
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  import Links._
  import Implicits._
  import Proxies._
  import lift._

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Lift.type]

  it should s"function properly with Hystrix" in {
    import Implicits._

    val foo =
      for {
        s <- p1 <~> p2 <~> p3 <~> (((l: Long) => { /*Thread.sleep(3000L);*/ l }, identity[Int] _))
        f <- s |> Serial() |> Hystrix.withFallback(config = HystrixConfig(timeout = 1.second))("boo!") |> Hystrix.withFallback(config = HystrixConfig(timeout = 1.second))("?") |> Async()
      } yield f
    Await.result(foo.run("0"), 10.seconds) should be ("33")
  }
}