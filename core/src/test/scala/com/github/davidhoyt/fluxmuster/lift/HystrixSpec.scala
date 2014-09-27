package com.github.davidhoyt.fluxmuster.lift

import com.github.davidhoyt.fluxmuster._
import com.netflix.hystrix.exception.HystrixRuntimeException
import org.scalatest.concurrent.ScalaFutures

class HystrixSpec extends UnitSpec with ScalaFutures {
  import org.scalatest.time.{Seconds, Span}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  import Links._
  import Implicits._
  import Proxies._

  implicit val timeout = 1.second
  implicit val patience = PatienceConfig(Span(3L, Seconds))

  val expectedError = new IllegalStateException(s"Intentionally thrown error (please disregard)")
  val proxy = p1 <~> p2 <~> p3
  val proxyWithError = proxy <~> ((x: Long) => if (true) throw expectedError else x, identity[Int]_).toProxy("Intentional Error")

  behavior of Macros.simpleNameOf[Hystrix.type]

  it must s"run correctly with a single ${Macros.simpleNameOf[Hystrix.type]} without a fallback" in {
    val lifted =
      proxy |>
        Hystrix[String, String](HystrixConfig())

    lifted.run("0").futureValue should be ("33")
  }

  it must s"run correctly with a single ${Macros.simpleNameOf[Hystrix.type]} with a fallback" in {
    val lifted =
      proxy |>
        Hystrix.withFallback(HystrixConfig())("?")

    lifted.run("0").futureValue should be ("33")
  }

  it must s"run correctly with a single ${Macros.simpleNameOf[Hystrix.type]} without a fallback and a failure" in {
    val lifted =
      proxyWithError |>
        Hystrix[String, String](HystrixConfig())

    lifted.run("0").failed.futureValue.getClass should be (classOf[HystrixRuntimeException])
  }

  it must s"run correctly with a single ${Macros.simpleNameOf[Hystrix.type]} with a fallback and a failure" in {
    val lifted =
      proxyWithError |>
        Hystrix.withFallback(HystrixConfig())("?")

    lifted.run("0").futureValue should be ("?")
  }

  it must s"propagate the fallback through multiple lifts" in {
    val lifted =
      proxyWithError |>
        Hystrix.withFallback(HystrixConfig())("?") |>
        Async() |>
        Hystrix[String, String](HystrixConfig()) |>
        Async()

    lifted.run("0").futureValue should be ("?")
  }

  it must s"catch errors and propagate it through multiple lifts even if a fallback is used at the end" in {
    val lifted =
      proxyWithError |>
        Hystrix[String, String](HystrixConfig()) |>
        Async() |>
        Async() |>
        Hystrix.withFallback(HystrixConfig())("?")

    lifted.run("0").failed.futureValue.getClass should be (classOf[HystrixRuntimeException])
  }

  it must s"run correctly with multiple lifts" in {
    val lifted =
      proxy |>
        Serial() |>
        Hystrix.withFallback(HystrixConfig())("?") |>
        Async() |>
        Hystrix.withFallback(HystrixConfig())("?") |>
        Async()

    lifted.run("0").futureValue should be ("33")
  }
}