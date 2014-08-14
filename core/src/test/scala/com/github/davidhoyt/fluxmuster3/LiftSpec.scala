package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}

import scala.concurrent.{Await, Future, ExecutionContext}

class LiftSpec extends UnitSpec {
  import scala.collection._
  import scala.language.existentials

  behavior of Macros.simpleNameOf[Lift.type]

  def stringToLong(x: String) = x.toLong
  def longToInt(x: Long) = x.toInt

  //Poly
  def polyToString[A](a: A) = a.toString
  def stringToInt(s: String) = s.toInt
  def liftToSeq[A](a: A) = Seq(a)
  def seqToHead[A](a: Seq[A]): A = a.head

  val linkStringIdentity = Link.identity[java.lang.String]
  val linkIntIdentity = Link.identity[Int]

  val linkIntToString: Link[Int, String] = polyToString[Int]_
  val linkStringToInt: Link[String, Int] = stringToInt _
  val linkIntToStringRepeated: Link[Int, String] = linkIntToString ~> linkStringToInt ~> linkIntToString ~> linkStringToInt ~> linkIntToString
  val linkLiftToSeqString: Link[String, Seq[String]] = liftToSeq[String] _
  val linkHeadSeqString: Link[Seq[String], String] = seqToHead[String] _
  val link6: Link[Int, Seq[String]] = linkIntToStringRepeated ~> linkLiftToSeqString
  val link7: Link[String, String] = linkIntToString <~ linkStringToInt //<~ link1 <~ link2
  val link8: Link[String, Seq[String]] = link7 ~> linkLiftToSeqString

  it should "have a proper toString()" in {
    //
  }

  it should "properly compose a series of downstream function links" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    //Should be able to specify upstream and downstream separately and
    //then combine then later.
    val s1 = Step.create("Step1")(linkIntToString ~> ((x: String) => "1" + x) ~> linkLiftToSeqString)(linkStringToInt <~ linkHeadSeqString)
    val s2 = ((x: Seq[String]) => x.head.toInt * 2).toStep
    val l1 = Async(s1)
    val r1 = l1(0)
    println(Await.result(r1, 10.seconds))
  }
}
