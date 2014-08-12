package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}

class StepSpec extends UnitSpec {
  import scala.collection._
  import scala.language.existentials

  behavior of Macros.simpleNameOf[Step.type]

  def stringToLong(x: String) = x.toLong
  def longToInt(x: Long) = x.toInt

  //Poly
  def polyToString[A](a: A) = a.toString
  def stringToInt(s: String) = s.toInt
  def liftToSeq[A](a: A) = Seq(a)
  def seqToHead[A](a: Seq[A]): A = a.head

  val linkStringIdentity = Link.identity[java.lang.String]

  val linkIntToString: Link[Int, String] = polyToString[Int]_
  val linkStringToInt: Link[String, Int] = stringToInt _
  val linkIntToStringRepeated: Link[Int, String] = linkIntToString ~> linkStringToInt ~> linkIntToString ~> linkStringToInt ~> linkIntToString
  val linkLiftToSeqString: Link[String, Seq[String]] = liftToSeq[String] _
  val linkHeadSeqString: Link[Seq[String], String] = seqToHead[String] _
  val link6: Link[Int, Seq[String]] = linkIntToStringRepeated ~> linkLiftToSeqString
  val link7: Link[String, String] = linkIntToString <~ linkStringToInt //<~ link1 <~ link2
  val link8: Link[String, Seq[String]] = link7 ~> linkLiftToSeqString

  it should "have a proper toString()" in {
    Step("BiDi1").toString should be (s"${Macros.simpleNameOf[EmptyStep.type]}(BiDi1)")
    (Step("BiDi2") ~> linkIntToString).toString should be (s"${Macros.simpleNameOf[Step.type]}(BiDi2)[${linkIntToString.typeIn.toShortString}, ${linkIntToString.typeOut.toShortString}, ${linkIntToString.typeOut.toShortString}, ${linkIntToString.typeOut.toShortString}]")
  }

  it should "properly compose a series of downstream function links" in {

    val b1 = Step("BiDi1") ~> linkIntToString ~> linkLiftToSeqString // <~ link1 <~ link2
    b1.downstream.chain should be (immutable.Seq(linkIntToString, linkLiftToSeqString))
    //Should be the following, but would require a new trait where the downstream
    //can be defined but upstream isn't. The opposite as well where upstream is
    //defined by downstream isn't. ~>/<~ off of BiDirectional should create those
    //instances.
    //
    //b1.upstream.chain should be (immutable.Seq(Link.identity[Seq[String]]))
    b1.upstream.chain should be (immutable.Seq(linkStringIdentity))

    val b2 = b1 <~ linkIntToString <~ linkStringToInt
    b2.downstream.chain should be (b1.downstream.chain)
    b2.upstream.chain should be (immutable.Seq(linkStringToInt, linkIntToString, linkStringIdentity))

    val b3: Step[Int, Seq[String], Seq[String], String] = b2 <~ linkHeadSeqString
    b3.downstream.chain should be (b1.downstream.chain)
    b3.upstream.chain should be (linkHeadSeqString +: b2.upstream.chain)
    b3(0) should be ("0")

    val b4 = Step("BiDi4") ~> linkHeadSeqString ~> linkStringToInt
    val b5 = Step.create("BiDi5")(linkHeadSeqString ~> linkStringToInt)(linkLiftToSeqString <~ linkIntToStringRepeated)
    val b6 = b3 combine b5
    b6(0) should be ("0")

    val b6link = b6.runLink
    b6link(0)(identity, identity) should be ("0")

  }
}
