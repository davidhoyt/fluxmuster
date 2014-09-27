package com.github.davidhoyt.fluxmuster

import akka.actor.ActorSystem
import akka.util.Timeout
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.{ScalaFutures, Futures}
import org.scalatest.time.{Seconds, Span}

class AkkaSpec extends UnitSpec with ScalaFutures with BeforeAndAfterAll {
  import scala.concurrent.Await
  import scala.concurrent.duration._

  import Links._
  import Implicits._
  import Proxies._
  import lift._

  implicit val system = ActorSystem("test")
  implicit val timeout = Timeout(1.second)
  implicit val patience = PatienceConfig(Span(3L, Seconds))
  import system.dispatcher

  val proxy = p1 <~> p2 <~> p3

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Lift.type]

  it should s"run serially with ${Macros.simpleNameOf[Akka.type]}" in {
    val lifted = proxy |> Akka(config = AkkaConfig())
    lifted.run("0").futureValue should be ("33")
  }

  it should s"run in parallel with ${Macros.simpleNameOf[Akka.type]}" in {
    val lifted = proxy |> Akka.par(config = AkkaConfig())
    lifted.run("0").futureValue should be ("33")
  }

  it should s"run mixing multiple ${Macros.simpleNameOf[Akka.type]} instances" in {
    val lifted = proxy |>
      Akka.par(config = AkkaConfig()) |>
      Akka.par(config = AkkaConfig()) |>
      Akka(config = AkkaConfig())

    lifted.run("0").futureValue should be ("33")
  }

  it should s"run mixing multiple lifts" in {
    val lifted = proxy |>
      Serial() |>
      Akka.par(config = AkkaConfig()) |>
      Async() |>
      Akka(config = AkkaConfig()) |>
      Async() |>
      Async()

    lifted.run("0").futureValue should be ("33")
  }

  override protected def afterAll(): Unit =
    system.shutdown()
}