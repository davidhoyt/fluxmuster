package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}

class BiDirectionalSpec extends UnitSpec {
  behavior of Macros.simpleNameOf[BiDirectional.type]

  def stringToLong(x: String) = x.toLong
  def longToInt(x: Long) = x.toInt

  //Poly
  def polyToString[A](a: A) = a.toString
  def stringToInt(s: String) = s.toInt
  def liftToSeq[A](a: A) = Seq(a)
  def seqToHead[A](a: Seq[A]): A = a.head

  val link1: Linked[Int, String] = polyToString[Int]_
  val link2: Linked[String, Int] = stringToInt _
  val link3: Linked[Int, String] = link1 ~> link2 ~> link1 ~> link2 ~> link1
  val link4: Linked[String, Seq[String]] = liftToSeq[String] _
  val link5: Linked[Seq[String], String] = seqToHead[String] _
  val link6: Linked[Int, Seq[String]] = link3 ~> link4
  val link7: Linked[String, String] = link1 <~ link2 //<~ link1 <~ link2
  val link8: Linked[String, Seq[String]] = link7 ~> link4

  it should "have a proper toString()" in {
    BiDirectional("BiDi1").toString should be (s"${Macros.simpleNameOf[EmptyBiDirectional.type]}(BiDi1)")
    (BiDirectional("BiDi2") ~> link1).toString should be (s"${Macros.simpleNameOf[BiDirectional.type]}(BiDi2)[${link1.typeIn.toShortString}, ${link1.typeOut.toShortString}, ${link1.typeOut.toShortString}, ${link1.typeOut.toShortString}]")
  }

  it should "properly compose a series of downstream function links" in {

    val b1 = BiDirectional("BiDi1") ~> link1 ~> link4 <~ link1 <~ link2
    val b2 = b1
    println(b1)
  }
}
