package com.github.davidhoyt.fluxmuster

import akka.actor.ActorSystem

class LiftSpec extends UnitSpec {
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.util.Success
  import Links._
  import Proxies._
  import lift._

  import scala.concurrent.ExecutionContext.Implicits.global

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Lift.type]

  it should s"implicitly convert to ${Macros.simpleNameOf[LinkedProxy.type]} and lift into multiple lifts" in {
    val l1 = p1 |> Serial("s1") |> Serial() |> Async("a1")
    val result = l1.run("0")
    l1.liftChain.map(_.name) should be(Vector("s1", Serial.defaultName, "a1"))
    Await.result(result, 10.seconds) should be("3")
  }

  it should s"allow composition without proof unless needed" in {
    val l1 =
      for {
        p1 <- Proxy("t1/t2.1", Link.identity[T1], Link.identity[T2])
        p2 <- Proxy("t1/t2.2", Link.identity[T1], Link.identity[T2])
        p3 <- p1 <~> p2
      } yield p3

    ////Following should fail to compile because an implicit T1 => T2 is
    ////not available.
    //val l2 =
    //  for {
    //    p <- l1 |> Serial()
    //  } yield p

    //Following compiles because an implicit has been provided in scope
    //to map from T1 to T2. Running it, though, should throw an exception.
    val l3 = {
      implicit def t1ToT2(t1: T1): T2 =
        ???

      for {
        p <- l1 lift Serial()
      } yield p
    }

    l3(null).isSuccess should be (false) //because ??? throws an exception
  }

  it should s"compose using symbolic operators" in {
    val l1 =
      for {
        pp1 <- p1
        pp2 <- p2
        pp3 <- p3
        proxy <- pp1 <~> pp2 <~> pp3
        _ <- pp1 <~> pp2 <~> pp3 |> Serial() //ignore
        _ <- p1 <~> p2 <~> p3 |> Async()     //ignore
      } yield proxy |> Serial("s1") |> Async("a1")

    l1.liftChain.map(_.name) should be(Vector("s1", "a1"))
    Await.result(l1.run("0"), 10.seconds) should be("33")
  }

  it should s"compose cleanly without syntactic sugar with multiple lifts" in {
    val l1 =
      p1 flatMap { a =>
        p2 flatMap { b =>
          p3 flatMap { c =>
            a combine b combine c lift Serial("s1") flatMap { d =>
              d lift Serial("s2") flatMap { e =>
                e |> Async("a3") flatMap { f =>
                  f |> Async("a4") map { g =>
                    g
                  }
                }
              }
            }
            //Serial("s1", a combine b combine c) flatMap { d =>
            //  Serial("s2", d) flatMap { e =>
            //    Async("a3", e) flatMap { f =>
            //      Async("a4", f) map { g =>
            //        //println(f.liftChain.map(_.name))
            //        g
            //      }
            //    }
            //  }
            //}
          }
        }
      }

    Await.result(l1.run("0"), 10.seconds) should be("33")
    l1.liftChain.map(_.name) should be(Vector("s1", "s2", "a3", "a4"))
  }

  it should s"compose with for comprehensions and ignore filters" in {
    val singleLiftDoNotUseAllProxies =
      for {
        pp1 <- p1 if false
        pp2 <- p2 if true
        pp3 <- p3 if false
        s1 <- pp1 <~> pp2 |> Serial("s1")
      } yield s1

    val result1 = singleLiftDoNotUseAllProxies.run("3")
    result1 should be (Success("25"))
    singleLiftDoNotUseAllProxies.liftChain.map(_.name) should be (Vector("s1"))

    val doubleLift =
      singleLiftDoNotUseAllProxies lift Serial("s2")
    doubleLift.liftChain.map(_.name) should be (Vector("s1", "s2"))

    val doubleLift2 =
      singleLiftDoNotUseAllProxies |> Async("a1")
    doubleLift2.liftChain.map(_.name) should be (Vector("s1", "a1"))

    val doubleLift3 =
      doubleLift2 |> Async("a2") |> Async("a3") |> Async("a4")
    doubleLift3.liftChain.map(_.name) should be (Vector("s1", "a1", "a2", "a3", "a4"))
    Await.result(doubleLift3.run("3"), 10.seconds) should be ("25")

    val simpleLiftMapWithLink =
      for {
        l1 <- linkS2L ~> linkInc2Mult1 lift Serial("s1")
      } yield l1

    val simpleLiftFlatMapDesugared =
      linkS2L ~> linkInc2Mult1 lift Serial("s1") flatMap { a =>
        a lift Serial("s2") map { b =>
          b
        }
      }

    val simpleLiftFlatMap =
      for {
        l1 <- linkS2L ~> linkInc2Mult1 |> Serial("r1")
        l2 <- l1 |> Serial("r2")
      } yield l2
  }
}