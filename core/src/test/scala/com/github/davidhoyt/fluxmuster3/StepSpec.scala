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
    Step("Step1").toString should be (s"${Macros.simpleNameOf[EmptyStep.type]}(Step1)")
    (Step("Step2") #> linkIntToString).toString should be (s"${Macros.simpleNameOf[DownstreamStep.type]}(Step2)[${linkIntToString.typeIn.toShortString}, ${linkIntToString.typeOut.toShortString}]")
    (Step("Step3") <# linkIntToString).toString should be (s"${Macros.simpleNameOf[UpstreamStep.type]}(Step3)[${linkIntToString.typeIn.toShortString}, ${linkIntToString.typeOut.toShortString}]")
  }

  it should "properly compose a series of downstream function links" in {

    //Should be able to specify upstream and downstream separately and
    //then combine then later.
    val s1 = Step("Step1") #> (linkIntToString ~> linkLiftToSeqString) // <~ link1 <~ link2
    val s2 = Step("Step2") <# (linkStringToInt <~ linkHeadSeqString)
    val s3 = s1 <# s2
    val s4 = s2 #> s1
    s3 should be (s4)
    s3.chain should be (s4.chain)
    s3(0) should be (0)
    s3(0) should be (s4(0))

    //Order should not matter if we specify upstream before downstream.
    val s5 = (Step("Step5") <# (linkStringToInt <~ linkHeadSeqString)) #> (linkIntToString ~> linkLiftToSeqString)
    s5 should be (s4)
    s5 should be (s3)
    s5.chain should be (s4.chain)
    s5.chain should be (s3.chain)
    s5(0) should be (0)
    s5(0) should be (s4(0))
    s5(0) should be (s3(0))

    //Order should not matter if we specify downstream before upstream.
    val s6 = (Step("Step6") #> (linkLiftToSeqString <~ linkIntToString)) <# (linkHeadSeqString ~> linkStringToInt)
    s6 should be (s5)
    s6 should be (s4)
    s6 should be (s3)
    s6.chain should be (s5.chain)
    s6(0) should be (0)

    //Test using DownstreamStep.~>(Link).
    val s7: DownstreamStep[Int, String] =
      Step("Step7") #> linkIntToString ~> linkLiftToSeqString ~> linkHeadSeqString

    //Test using UpstreamStep.<~(Link).
    val s8: UpstreamStep[Int, String] =
      Step("Step8") <# linkHeadSeqString <~ linkLiftToSeqString <~ linkIntToString

    //Test using DownstreamStep.<#(Link) and Link.~>(Link).
    //Parenthesis are needed because the Link must be formed so that
    //implicits can be resolved when it links to the DownstreamStep instance.
    //If you're curious, remove the parenthesis and try and understand the
    //compile error.
    val s9 =
      Step("Step9") #> linkIntToString ~> linkLiftToSeqString ~> linkHeadSeqString <# (linkIntIdentity <~ linkStringToInt)

    //Test using DownstreamStep.<#(Link) and Link.<~(Link).
    val s10 =
      Step("Step10") #> linkIntToString ~> linkLiftToSeqString ~> linkHeadSeqString <# linkStringToInt ~> linkIntIdentity
    s10 should be (s9)
    s10.chain should be (s9.chain)

    val s11upstream =
      Step("Step11") <# (linkIntIdentity <~ linkStringToInt)
    val s11downstream =
      Step("Step11") #> linkIntToString ~> linkLiftToSeqString ~> linkHeadSeqString
    val s11 = s11upstream #> s11downstream
    val s12 = s11downstream <# s11upstream
    s11 should be (s12)
    s11.chain should be (s12.chain)
    s11.downstream.chain should be (s11downstream.link.chain)
    s11.upstream.chain should be (s11upstream.link.chain)

    //Tuple2 with functions can be converted to a step.
    val s13 = ((x: Int) => x.toString, (x: String) => x.toInt + 1).toStep
    val s14 = (linkIntToString ~> linkLiftToSeqString, linkHeadSeqString ~> linkStringToInt).toStep

    //val s10 = s7

//    b1.downstream.chain should be (immutable.Seq(linkIntToString, linkLiftToSeqString))
//    //Should be the following, but would require a new trait where the downstream
//    //can be defined but upstream isn't. The opposite as well where upstream is
//    //defined by downstream isn't. ~>/<~ off of BiDirectional should create those
//    //instances.
//    //
//    //b1.upstream.chain should be (immutable.Seq(Link.identity[Seq[String]]))
//    b1.upstream.chain should be (immutable.Seq(linkStringIdentity))
//
//    //val foo = linkHeadSeqString <~ linkLiftToSeqString <~ linkIntToString <~ linkStringToInt
//    val b2: Step[Seq[String], Seq[String], Seq[String], String] = Step("Step2") <# (linkIntToString <~ linkStringToInt <~ linkHeadSeqString)
//    val b3 = b1 combine b2
//    b3.downstream.chain should be (b1.downstream.chain)
//    b3.upstream.chain should be (immutable.Seq(linkHeadSeqString, linkLiftToSeqString, linkStringToInt, linkIntToString, linkStringIdentity))
//    b3(0) should be ("0")
//
//    val b4 = Step("Step4") #> (linkHeadSeqString ~> linkStringToInt)
//    val b5 = Step.create("Step5")(linkHeadSeqString ~> linkStringToInt)(linkLiftToSeqString <~ linkIntToStringRepeated)
//    val b6 = b3 combine b4
//    b6(0) should be ("0")
//
//    val b6link = b6.toLink
//    b6link(0)(identity, identity) should be ("0")
  }
}
