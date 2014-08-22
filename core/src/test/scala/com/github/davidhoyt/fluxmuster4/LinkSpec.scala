package com.github.davidhoyt.fluxmuster4

import lift.{HystrixConfiguration, Hystrix}

import com.github.davidhoyt.fluxmuster.{Macros, UnitSpec}

import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

class LinkSpec extends UnitSpec {
  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Link.type]

  def stringToLong(x: String) = x.toLong
  implicit def longToInt(x: Long) = x.toInt

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
    link1.chain.length should be (4)
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
    //It includes implicit conversions between links. If the conversion would be an identity
    //(which would occur when the output type of link A is the input type of link B), then
    //those are discarded in order to reduce the length of the chain.
    link1.chain.length should be (4)
  }

  it should "handle map" in {
    val link1: Link[Int, Long] =
      ((x: Int) => x + 1) ~>
      ((x: Long) => x + 1L)

    val sugared: Link[Int, String] =
      for {
        a <- link1
      } yield a.toString

    //This results in a call to map which should add to
    //the chain and not replace it. Since link1's chain size
    //is 3 (2 links + implicit conversion Int => Long), then
    //the additional map will add another 1 (implicit identity
    //and the expression in the yield as a new link, but the
    //identity is rooted out when detected).
    sugared.chain.length should be (4)

    sugared.run(0) should be ("2")

    //Call it normally.
    val sugared2 = link1 map (_.toString)
    sugared2.chain.length should be (4)
    sugared2.run(0) should be ("2")
  }

  it should "handle foreach" in {
    val link1: Link[Int, Long] =
      ((x: Int) => x + 1) ~>
      ((x: Long) => x + 1L)

    var sideEffect1 = 0
    val sugared: Link[Int, Long] =
      for (a <- link1)
        sideEffect1 = 111

    //This results in a call to foreach which should not modify
    //the chain at all. Instead it will append to the link's
    //side effect chain.

    sugared.sideEffects.length should be (1)
    sugared.chain.length should be (3)
    sugared.run(0) should be (2)
    sideEffect1 should be (111)

    //Call it normally.
    var sideEffect2 = 0
    var sideEffect3 = 0
    val sugared2 = link1 foreach { _ => sideEffect2 = 111 } foreach { _ => sideEffect3 = 222 }
    sugared2.sideEffects.length should be (2)
    sugared2.chain.length should be (3)
    sugared2.run(0) should be (2)
    sideEffect2 should be (111)
    sideEffect3 should be (222)
  }

  it should "handle flatMap" in {
    val link1: Link[Int, Long] =
      ((x: Int) => x + 1) ~>
      ((x: Long) => x + 1L)

    val link2: Link[Long, Long] =
      ((x: Long) => x + 2L) ~>
      ((x: Long) => x * 1L)

    val sugared: Link[Int, Long] =
      for {
        a <- link1 //Output of this
        b <- link2 //feeds into the input of this
      } yield b //a = 2, b = 4, c = 2 if we call sugared with 0

    //This results in a call to flatMap which will run link2 inside of
    //link1 and thus flatten the chain. Important to be aware of this
    //when lifting something for parallel execution.
    sugared.chain.length should be (1)

    sugared.run(0) should be (4)

    //Call it normally.
    val sugared2 =
      link1 flatMap { a =>
        link2 flatMap { b =>
          link1 map { c =>
            b
          }
        }
      }
    sugared2.chain.length should be (1)
    sugared2.run(0) should be (4)
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

    val step3fromApply = step3.run(1)
    val step3fromChain = step3.runChain(1)

    step3fromApply should be (2)
    step3fromApply should be (step3fromChain)

    val foo =
      for {
        (down, up) <- step3
      } yield (down map (_.toInt), up.map(x => x.toLong))



//    val foo =
//      for {
//        (down, up) <- (step1 combine step2) if down ne null
//      } yield {
//        (down ~> identity[Long]_, up)
//      }
  }

  def executeLiftMultipleTimes[D, S](lift: Lift[Int, D, S, Future], times: Int = 250, timeout: FiniteDuration = 10.seconds)(implicit context: ExecutionContext): Seq[D] = {
    val sequenced = Future.sequence[D, Seq](
      for (idx <- 0 until times)
      yield lift.run(idx)
    )
    Await.result(sequenced, timeout)
  }

  it should "lift" in {

    val link1: Link[Int, Long] =
      ((x: Int) => x + 1).toLink("link1-a") ~>
      ((x: Long) => (x + 1L)).toLink("link1-b")

    val link2: Link[Long, Long] =
      ((x: Long) => x + 2L).toLink("link2-a") ~>
      ((x: Long) => x * 2L).toLink("link2-b")

    val step1 =
      Step("step1", link1, link2, identity[Long]_)

    val liftLink1 = Async(link1)
    val liftLink2 = Async(link2)
    val liftLink3 = Async(step1)
    val liftCombined = liftLink1 andThen liftLink2 andThen liftLink1 andThen liftLink3 andThen link1 andThen link1 andThen step1



    //println(liftCombined.chain.asDefaultString)
    val Seq(result, chainedResult) =
      Await.result(Future.sequence(Seq(liftCombined(0), liftCombined.runChain(0))), 5.seconds)
    chainedResult should be (result)
    result should be (72)

    //Double check that it can support multiple thousands of concurrent runs.
    executeLiftMultipleTimes(liftCombined, times = 2000)

    val liftLink4 = Hystrix.withFallback(HystrixConfiguration(timeout = 1.second))(-1L) |> link1
    val resultLiftLink4 = liftLink4(0)

    it should "foo" in {
    withClue("Hystrix fallback should not be used: ") {
      executeLiftMultipleTimes(liftLink4, times = 1000) contains (-1L) should be(false)
    }}
  }
}
