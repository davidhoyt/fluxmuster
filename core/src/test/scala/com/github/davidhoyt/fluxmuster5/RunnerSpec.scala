package com.github.davidhoyt.fluxmuster5

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}
import com.github.davidhoyt.fluxmuster5.runner.Serial

import scala.util.Try

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

  implicit def intToString(x: Int) = x.toString
  implicit def longToInt(x: Long) = x.toInt

  it should "do this" in {
    val bar =
      Proxy("p1", link2 ~> link3, link4) flatMap { a =>
        Proxy("p2", link3, link4) flatMap { b =>
          Serial(b)
        }
      }

    val foo =
      for {
        p1: ProxyNeedsProof[String, Long, Int, Int] <- Proxy("p1", link2 ~> link3, link4)
        p2: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p2", link3, link4)
        r1 <- Serial(p2)
      } yield r1

    println(foo.chain.asDefaultString)
    val r = foo.run("0")
    println(r)
  }
}