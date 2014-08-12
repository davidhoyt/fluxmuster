package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{UnitSpec, Macros}

class LinkSpec extends UnitSpec {
  behavior of Macros.simpleNameOf[Link.type]

  def stringToLong(x: String) = x.toLong
  def longToInt(x: Long) = x.toInt

  //Poly
  def createString[A](a: A) = a.toString

  it should "properly compose a series of downstream function links" in {
    val link1 = createString[Int]_ ~> ((x: String) => x.toLong) down ((x: Long) => Seq(x))

    val result =
      for (i <- 0 to 10)
        yield link1(i)

    val expected =
      for (i <- 0 to 10)
        yield Seq(i)

    result should be (expected)
    link1.chain.length should be (3)
  }

  it should "properly compose a series of upstream function links" in {
    val link1: Link[Int, Seq[Long]] = ((x: Long) => Seq(x)) up ((x: String) => x.toLong) <~ createString[Int]_

    val result =
      for (i <- 0 to 10)
        yield link1(i)

    val expected =
      for (i <- 0 to 10)
        yield Seq(i)

    result should be (expected)
    link1.chain.length should be (3)
  }
}
