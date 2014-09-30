package com.github.davidhoyt.fluxmuster

class LinkSpec extends UnitSpec {
  import Links._

  behavior of Macros.simpleNameOf[Link.type]

  it should "correctly tee values" in {
    linkS2Sum.run("0") should be (3L)
  }
}