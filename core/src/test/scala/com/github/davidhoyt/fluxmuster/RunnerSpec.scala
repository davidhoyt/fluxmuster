package com.github.davidhoyt.fluxmuster

class RunnerSpec extends UnitSpec {
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.util.Success
  import Links._
  import runner._

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Runner.type]

  it should "do this" in {
    import scala.concurrent.ExecutionContext.Implicits.global

    val bar =
      Proxy("p1", linkS2L ~> linkInc2Mult1, linkMult2Dec1 ~> linkI2S) flatMap { a =>
        Proxy("p2", linkInc2Mult1, linkMult2Dec1) flatMap { b =>
          Proxy("p3", linkInc1, linkMult2Dec1) flatMap { c =>
            Serial("s1", a combine b combine c) flatMap { d =>
              Serial("s2", d) map { e =>
                Async("a3", e) map { f =>
//                  Async("a4", c) map { g =>
//                    println(f.runnerChain.map(_.name))
//                    g
//                  }
                  f
                }
//                e
              }
//              d
            }
          }
        }
      }

    Await.result(bar.run("0"), 10.seconds) should be ("33")
    //println(bar.asDefaultString)

    val foo =
      for {
        p1: ProxyNeedsProof[String, Long, Int, String] <- Proxy("p1", linkS2L ~> linkInc2Mult1, linkMult2Dec1 ~> linkI2S) if false
        p2: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p2", linkInc2Mult1, linkMult2Dec1) if true
        p3: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p3", linkInc1, linkMult2Dec1) if false
        //Only thing we know is DownstreamOut and UpstreamIn at this point
        //Same for subsequent Runners (Lifts)
        r1 <- Serial("r1", p1 combine p2)
        //r2 <- Serial("r2", r1)
      } yield r1

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

    //println(foo.chain.asDefaultString)
    val r = foo.run("3")
    r should be (Success("25")) //TODO: Check that
  }
}