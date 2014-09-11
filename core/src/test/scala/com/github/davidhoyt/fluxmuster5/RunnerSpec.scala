package com.github.davidhoyt.fluxmuster5

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}
import com.github.davidhoyt.fluxmuster5.runner.{Async, Serial}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Try}

class RunnerSpec extends UnitSpec {

  import scala.language.implicitConversions

  import scala.concurrent.ExecutionContext.Implicits.global

  behavior of Macros.simpleNameOf[Link.type]

  val link2: Link[String, Long] =
    ((x: String) => {println(s"link2-a: '$x'"); x.toLong}).toLink("link2-a")

  val link3: Link[Long, Long] =
    ((x: Long) => {println(s"link3-a: $x"); x + 2L}).toLink("link3-a") ~>
    ((x: Long) => {println(s"link3-b: $x"); x * 1L}).toLink("link3-b")

  val link4: Link[Int, Int] =
    ((x: Int) => {println(s"link4-a: $x"); x * 2}).toLink("link4-a") ~>
    ((x: Int) => {println(s"link4-b: $x"); x - 1}).toLink("link4-b")

  val link5: Link[Int, String] =
    ((x: Int) => {println(s"link5-a: $x"); x.toString}).toLink("link5-a")

  val link6: Link[Long, Long] =
    ((x: Long) => {println(s"link6-a: $x"); x + 1L}).toLink("link6-a")

  implicit def intToString(x: Int) = x.toString
  implicit def longToInt(x: Long) = x.toInt

  it should "do this" in {
//    val baz =
//      Serial(link2 ~> link3)
//    println(baz.run("0"))
//
    val bar =
      Proxy("p1", link2 ~> link3, link4 ~> link5) flatMap { a =>
        Proxy("p2", link3, link4) flatMap { b =>
          Proxy("p3", link6, link4) flatMap { c =>
            Serial("s1", c) flatMap { d =>
              Serial("s2", d) flatMap { e =>
                Async("a3", e) map { f =>
                  Async("a4", c) map { g =>
//                    println(f.runnerChain.map(_.name))
                    g
                  }
//                  f
                }
              }
//              d
            }
          }
        }
      }
    println(bar.asDefaultString)
    val b = bar.run("0")
    println(b)
    println(Await.result(b, 10.seconds))
    println("$$$$$$$$$$$$$$$")
    System.exit(0)

    val foo =
      for {
        p1: ProxyNeedsProof[String, Long, Int, String] <- Proxy("p1", link2 ~> link3, link4 ~> link5)
        p2: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p2", link3, link4)
        p3: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p3", link6, link4)
        //Only thing we know is DownstreamOut and UpstreamIn at this point
        //Same for subsequent Runners (Lifts)
        r1 <- Serial("r1", p2) //TODO: FIX! Should not be applying link6!!
        //r2 <- Serial("r2", r1)
      } yield r1
    println(s"runnerSpec: $foo")

    val simpleRunnerMapWithLink =
      for {
        r1 <- Serial("s1", link2 ~> link3)
      } yield r1

    val simpleRunnerFlatMapDesugared =
      Serial("s1", link2 ~> link3) flatMap { a =>
        Serial("s2", a) map { b =>
          b
        }
      }

    val simpleRunnerFlatMap =
      for {
        r1 <- Serial("r1", link2 ~> link3)
        r2 <- Serial("r2", r1)
      } yield r2

    println(foo.chain.asDefaultString)
    val r = foo.run("3")
    r should be (Success("41"))
  }
}