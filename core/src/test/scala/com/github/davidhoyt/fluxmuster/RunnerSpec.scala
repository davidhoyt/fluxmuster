package com.github.davidhoyt.fluxmuster

class RunnerSpec extends UnitSpec {
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.util.Success
  import Links._
  import Proxies._
  import runner._

  import scala.concurrent.ExecutionContext.Implicits.global

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Runner.type]

  it should s"implicitly convert to ${Macros.simpleNameOf[Proxy.type]} and lift into multiple runners" in {
    val r1 = p1 |> Serial("s1") |> Serial() |> Async("a1")
    val result = r1.run("0")
    r1.runnerChain.map(_.name) should be(Vector("s1", Serial.defaultName, "a1"))
    Await.result(result, 10.seconds) should be("3")
  }

  it should s"compose using symbolic operators" in {
    val r1 =
      for {
        pp1 <- p1
        pp2 <- p2
        pp3 <- p3
        proxy <- pp1 <~> pp2 <~> pp3
        _ <- pp1 <~> pp2 <~> pp3 |> Serial() //ignore
        _ <- p1 <~> p2 <~> p3 |> Async()     //ignore
      } yield proxy |> Serial("s1") |> Async("a1")

    r1.runnerChain.map(_.name) should be(Vector("s1", "a1"))
    Await.result(r1.run("0"), 10.seconds) should be("33")
  }

  it should s"compose cleanly without syntactic sugar with multiple runners" in {
    val r1 =
      p1 flatMap { a =>
        p2 flatMap { b =>
          p3 flatMap { c =>
            Serial("s1", a combine b combine c) flatMap { d =>
              Serial("s2", d) flatMap { e =>
                Async("a3", e) flatMap { f =>
                  Async("a4", f) map { g =>
                    //println(f.runnerChain.map(_.name))
                    g
                  }
                }
              }
            }
          }
        }
      }

    Await.result(r1.run("0"), 10.seconds) should be("33")
    r1.runnerChain.map(_.name) should be(Vector("s1", "s2", "a3", "a4"))
  }

  it should s"compose with for comprehensions and ignore filters" in {
    val singleRunnerDoNotUseAllProxies =
      for {
        pp1 <- p1 if false
        pp2 <- p2 if true
        pp3 <- p3 if false
        s1 <- Serial("s1", pp1 <~> pp2)
      } yield s1

    val result1 = singleRunnerDoNotUseAllProxies.run("3")
    result1 should be (Success("25"))
    singleRunnerDoNotUseAllProxies.runnerChain.map(_.name) should be (Vector("s1"))

    val doubleRunner =
      singleRunnerDoNotUseAllProxies lift Serial("s2")
    doubleRunner.runnerChain.map(_.name) should be (Vector("s1", "s2"))

    val doubleRunner2 =
      singleRunnerDoNotUseAllProxies |> Async("a1")
    doubleRunner2.runnerChain.map(_.name) should be (Vector("s1", "a1"))

    val doubleRunner3 =
      doubleRunner2 |> Async("a2") |> Async("a3") |> Async("a4")
    doubleRunner3.runnerChain.map(_.name) should be (Vector("s1", "a1", "a2", "a3", "a4"))
    Await.result(doubleRunner3.run("3"), 10.seconds) should be ("25")

    val simpleRunnerMapWithLink =
      for {
        r1 <- Serial("s1", linkS2L ~> linkInc2Mult1)
      } yield r1

    val simpleRunnerFlatMapDesugared =
      Serial("s1", linkS2L ~> linkInc2Mult1) flatMap { a =>
        Serial("s2", a) map { b =>
          b
        }
      }

    val simpleRunnerFlatMap =
      for {
        r1 <- Serial("r1", linkS2L ~> linkInc2Mult1)
        r2 <- Serial("r2", r1)
      } yield r2
  }

  it should s"function properly with Hystrix" in {
    val foo =
      for {
        s <- p1 <~> p2 <~> p3 <~> Proxy("a", (l: Long) => { Thread.sleep(1000L); l }, identity[Int] _)
        f <- s |> Hystrix.withFallback(configuration = HystrixConfiguration())("boo!")
      } yield f
    Await.result(foo.run("0"), 2.seconds) should be ("33")
  }
}