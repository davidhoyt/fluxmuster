package com.github.davidhoyt.fluxmuster

class ProxySpec extends UnitSpec {
  import Links._

  behavior of Macros.simpleNameOf[Proxy.type]

  it should "correctly return constructed proxies" in {

    val bar =
      Proxy("p1", linkS2L ~> linkInc2Mult1, linkMult2Dec1 ~> linkI2S) flatMap { a =>
        Proxy("p2", linkInc2Mult1, linkMult2Dec1) flatMap { b =>
          Proxy("p3", linkInc1, linkMult2Dec1) map { c =>
            a combine b combine c
          }
        }
      }
    bar.run("0") should be ("33")

    val foo =
      for {
        p1: ProxyNeedsProof[String, Long, Int, String] <- Proxy("p1", linkS2L ~> linkInc2Mult1, linkMult2Dec1 ~> linkI2S) if false
        p2: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p2", linkInc2Mult1, linkMult2Dec1) if true
        p3: ProxyNeedsProof[Long, Long, Int, Int] <- Proxy("p3", linkInc1, linkMult2Dec1) if false
        p4 <- p1 combine p2 if true
      } yield p4
    foo.run("0") should be ("13")
    //println(foo.chain.asDefaultString)

    val baz =
      for {
        fromTuple <- (linkInc1, linkMult2Dec1).toProxy("fromTuple")
      } yield foo combine fromTuple
    baz.run("0") should be ("33")
    println(baz.chain.asDefaultString)
  }
}