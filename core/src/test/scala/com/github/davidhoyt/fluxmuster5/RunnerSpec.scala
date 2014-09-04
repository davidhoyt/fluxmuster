package com.github.davidhoyt.fluxmuster5

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}
import com.github.davidhoyt.fluxmuster5.runner.Serial

import scala.util.{Success, Try}

class RunnerSpec extends UnitSpec {

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Link.type]

  val link2: Link[String, Long] =
    ((x: String) => {println("link2-a"); x.toLong}).toLink("link2-a")

  val link3: Link[Long, Long] =
    ((x: Long) => {println("link3-a"); x + 2L}).toLink("link3-a") ~>
    ((x: Long) => {println("link3-b"); x * 1L}).toLink("link3-b")

  val link4: Link[Int, Int] =
    ((x: Int) => {println("link4-a"); x * 2}).toLink("link4-a") ~>
    ((x: Int) => {println("link4-b"); x - 1}).toLink("link4-b")

  val link5: Link[Int, String] =
    ((x: Int) => {println("link5-a"); x.toString}).toLink("link5-a")

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
          Serial(b) flatMap { c =>
            Serial(c) map { d =>
              d
            }
          }
        }
      }

    val foo =
      for {
        p1: ProxyNeedsProof[String, Long, Int, String] <- Proxy("p1", link2 ~> link3, link4 ~> link5) if true
        p2: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p2", link3, link4)
        p3: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p3", link3, link4)
        //Only thing we know is DownstreamOut and UpstreamIn at this point
        //Same for subsequent Runners (Lifts)
        r1 <- Serial(p2)
        r2 <- Serial(r1)
      } yield r2
    println(s"runnerSpec: $foo")

    val simpleRunnerMapWithLink =
      for {
        r1 <- Serial(link2 ~> link3)
      } yield r1

    val simpleRunnerFlatMapDesugared =
      Serial(link2 ~> link3) flatMap { a =>
        Serial(a) map { b =>
          b
        }
      }

    val simpleRunnerFlatMap =
      for {
        r1 <- Serial(link2 ~> link3)
        r2 <- Serial(r1)
      } yield r2

    println(foo.chain.asDefaultString)
    val r = foo.run("0")
    r should be (Success("41"))
    val r2 = foo.runChain("0")
    r2 should be ("41")
  }
}