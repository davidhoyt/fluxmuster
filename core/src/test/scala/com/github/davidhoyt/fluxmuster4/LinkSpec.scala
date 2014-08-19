package com.github.davidhoyt.fluxmuster4

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}

class LinkSpec extends UnitSpec {
  behavior of Macros.simpleNameOf[Link.type]

  def stringToLong(x: String) = x.toLong
  def longToInt(x: Long) = x.toInt

  //Poly
  def createString[A](a: A) = a.toString

  it should "properly compose a series of downstream function links" in {
    val link1: Link[Int, Seq[Long]] = createString[Int]_ ~> ((x: String) => x.toLong) down ((x: Long) => Seq(x))

    val result =
      for (i <- 0 to 10)
        yield link1(i)

    val expected =
      for (i <- 0 to 10)
        yield Seq(i)

    result should be (expected)
    link1.chain.length should be (5)
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
    //It includes implicit conversions between links which are often just identity (no conversion).
    link1.chain.length should be (5)
  }

  it should "step" in {
    val link1: Link[Int, Long] =
      ((x: Int) => x + 1).toLink("link1-a") ~>
      ((x: Long) => (x + 1L)).toLink("link1-b")

    val link2: Link[String, Int] =
      ((x: String) => "2" + x).toLink("link2-a") ~>
      ((x: String) => x.toDouble).toLink("link2-b") ~>
      ((x: Double) => x / 10.0).toLink("link2-c") ~>
      ((x: Double) => x.toInt).toLink("link2-d") ~>
      ((x: Int) => x).toLink("link2-e")

    val step1: StepLike[Int, Long, String, Int] = Step("foo", link1, link2)

    val link3: Link[Long, Long] =
      ((x: Long) => x + 2L).toLink("link3-a") ~>
      ((x: Long) => x * 1L).toLink("link3-b")
    val link4: Link[Int, Int] =
      ((x: Int) => x * 2).toLink("link4-a") ~>
      ((x: Int) => x - 1).toLink("link4-b")

    val step2: StepLike[Long, Long, Int, Int] = Step("step2", link3, link4)

    implicit val int2string: Int => String = (x: Int) => x.toString
    implicit val long2int: Long => Int = (x: Long) => x.toInt
    val step3 = step1 combine step2
    println("---------")
    println(step3.chain.map(x => x.asDefaultString))

    val step3fromApply = step3.run(1)
    val step3fromChain = step3.runChain(1)
    println("---------")
    println(step3fromApply)
    println(step3fromChain)
  }
}
