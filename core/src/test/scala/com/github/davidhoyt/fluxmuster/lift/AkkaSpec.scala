package com.github.davidhoyt.fluxmuster.lift

import akka.actor.ActorSystem
import akka.util.Timeout
import com.github.davidhoyt.fluxmuster._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}

class AkkaSpec extends UnitSpec with ScalaFutures with BeforeAndAfterAll {
  import Links._
  import Proxies._

  import scala.concurrent.duration._

  implicit val system = ActorSystem("test")
  implicit val timeout = Timeout(1.second)
  implicit val patience = PatienceConfig(Span(3L, Seconds))
  import system.dispatcher

  val proxy = p1 <~> p2 <~> p3

  import scala.language.implicitConversions

  behavior of Macros.simpleNameOf[Akka.type]

  it should s"run serially with ${Macros.simpleNameOf[Akka.type]}" in {
    val lifted = proxy |> Akka(AkkaConfig())
    lifted.run("0").futureValue should be ("33")
  }

  it should s"run in parallel with ${Macros.simpleNameOf[Akka.type]}" in {
    val lifted = proxy |> Akka.par(AkkaConfig())
    lifted.run("0").futureValue should be ("33")
  }

  it should s"run mixing multiple ${Macros.simpleNameOf[Akka.type]} instances" in {
    val lifted =
      proxy |>
        Akka.par(AkkaConfig()) |>
        Akka.par(AkkaConfig()) |>
        Akka(AkkaConfig())

    lifted.run("0").futureValue should be ("33")
  }

  it should s"run mixing multiple lifts" in {
    val lifted =
      proxy |>
        Serial() |>
        Akka.par(AkkaConfig()) |>
        Async() |>
        Akka(AkkaConfig()) |>
        Async() |>
        Async()

    lifted.run("0").futureValue should be ("33")
  }

  override protected def afterAll(): Unit =
    system.shutdown()
}