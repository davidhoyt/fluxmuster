package com.github.davidhoyt.fluxmuster

class ProxySpec extends UnitSpec {
  import Chains._
  import Implicits._
  import Links._
  import TestUtils._

  behavior of Macros.simpleNameOf[Proxy.type]

  it should "correctly return constructed proxies" in {

    val desugared =
      Proxy("p1", linkS2L ~> linkInc2Mult1, linkMult2Dec1 ~> linkI2S) flatMap { a =>
        val q = Proxy("p2", linkInc2Mult1, linkMult2Dec1) flatMap { b =>
          Proxy("p3", linkInc1, linkMult2Dec1) map { c =>
            a combine b combine c
          }
        }
        q
      }

    desugared.run("0") should be ("33")
    val expectedDesugaredApproximateLinkChain =
      newLinkChain(linkS2L, linkInc2, linkMult1, linkInc2, linkMult1, linkInc1, /* (longToInt _).toLink, */ linkMult2, linkDec1, linkMult2, linkDec1, linkMult2, linkDec1, linkI2S)
    desugared.linkChain containsValuesInApproximateOrder expectedDesugaredApproximateLinkChain should be (true)

    val onlyCombinesSpecifiedProxies =
      for {
        p1: Proxy[String, Long, Int, String] <- Proxy("p1", linkS2L ~> linkInc2Mult1, linkMult2Dec1 ~> linkI2S) if false
        p2: Proxy[Long, Long, Int, Int] <- Proxy("p2", linkInc2Mult1, linkMult2Dec1) if true
        //ignored
        p3: Proxy[Long, Long, Int, Int] <- Proxy("p3", linkInc1, linkMult2Dec1) if false
        p4 <- p1 combine p2 if true
      } yield p4
    onlyCombinesSpecifiedProxies.run("0") should be ("13")
    onlyCombinesSpecifiedProxies.linkChain containsValuesInApproximateOrder newLinkChain(linkS2L, linkInc2, linkMult1, linkInc2, linkMult1, /* (longToInt _).toLink, */ linkMult2, linkDec1, linkMult2, linkDec1, linkI2S) should be (true)
    //println(onlyCombinesSpecifiedProxies.chain.asDefaultString)

    val combineWithTupled =
      for {
        fromTuple <- (linkInc1, linkMult2Dec1).toLinkedProxy("fromTuple")
      } yield onlyCombinesSpecifiedProxies combine fromTuple
    combineWithTupled.run("0") should be ("33")
    combineWithTupled.linkChain containsValuesInApproximateOrder newLinkChain(linkS2L, linkInc2, linkMult1, linkInc2, linkMult1, linkInc1, /* (longToInt _).toLink, */ linkMult2, linkDec1, linkMult2, linkDec1, linkMult2, linkDec1, linkI2S) should be (true)
    combineWithTupled.linkChain containsValuesInApproximateOrder expectedDesugaredApproximateLinkChain should be (true)
    //combineWithTupled should be (desugared)
  }
}