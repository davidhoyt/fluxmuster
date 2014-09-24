package com.github.davidhoyt.fluxmuster

object Proxies {
  import com.typesafe.scalalogging.Logger
  import org.slf4j.LoggerFactory
  import Links._

  import scala.language.implicitConversions

  private val logger = Logger(LoggerFactory.getLogger(Macros.simpleNameOf[Proxies.type]))

  trait T1
  trait T2
  trait T3
  trait T4

  val p1: Proxy[String, Long, Int, String] =
    Proxy("p1", linkS2L ~> linkInc2Mult1, linkMult2Dec1 ~> linkI2S)

  val p2: Proxy[Long, Long, Int, Int] =
    Proxy("p2", linkInc2Mult1, linkMult2Dec1)

  val p3: Proxy[Long, Long, Int, Int] =
    Proxy("p3", linkInc1, linkMult2Dec1)
}
