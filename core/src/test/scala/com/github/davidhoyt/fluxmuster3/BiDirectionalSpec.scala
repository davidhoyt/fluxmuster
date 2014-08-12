package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}

class BiDirectionalSpec extends UnitSpec {
  import scala.collection._
  import scala.language.existentials

  behavior of Macros.simpleNameOf[BiDirectional.type]

  def stringToLong(x: String) = x.toLong
  def longToInt(x: Long) = x.toInt

  //Poly
  def polyToString[A](a: A) = a.toString
  def stringToInt(s: String) = s.toInt
  def liftToSeq[A](a: A) = Seq(a)
  def seqToHead[A](a: Seq[A]): A = a.head

  val linkStringIdentity = Link.identity[java.lang.String]

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

    val b1 = BiDirectional("BiDi1") ~> link1 ~> link4 // <~ link1 <~ link2
    b1.downstream.chain should be (immutable.Seq(link1, link4))
    //Should be the following, but would require a new trait where the downstream
    //can be defined but upstream isn't. The opposite as well where upstream is
    //defined by downstream isn't. ~>/<~ off of BiDirectional should create those
    //instances.
    //
    //b1.upstream.chain should be (immutable.Seq(Link.identity[Seq[String]]))
    b1.upstream.chain should be (immutable.Seq(linkStringIdentity))

    val b2 = b1 <~ link1 <~ link2
    b2.downstream.chain should be (b1.downstream.chain)
    b2.upstream.chain should be (immutable.Seq(link2, link1, linkStringIdentity))

    val b3: BiDi[Int, Seq[String], Seq[String], String] = b2 <~ link5
    b3.downstream.chain should be (b1.downstream.chain)
    b3.upstream.chain should be (link5 +: b2.upstream.chain)
    b3(0) should be ("0")

    val b4 = BiDirectional("BiDi4") ~> link5 ~> link2
    println(b4)
    val b5 = BiDirectional.create("BiDi5")(link5 ~> link2)(link4 <~ link3)
    val b6 = b3 combine b5
    println(b5)

  }
}
