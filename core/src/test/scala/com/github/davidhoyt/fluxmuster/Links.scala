package com.github.davidhoyt.fluxmuster

object Links {
  import com.typesafe.scalalogging.Logger
  import org.slf4j.LoggerFactory

  import scala.language.implicitConversions

  private val logger = Logger(LoggerFactory.getLogger(Macros.simpleNameOf[Links.type]))

  val linkS2L: Link[String, Long] =
    ((x: String) => {logger.debug(s"linkS2L-a: '$x'.toLong"); x.toLong}).toLink("linkS2L-a")

  val linkInc2Mult1: Link[Long, Long] =
    ((x: Long) => {logger.debug(s"linkInc2Mult1-a: $x + 2L"); x + 2L}).toLink("linkInc2Mult1-a") ~>
    ((x: Long) => {logger.debug(s"linkInc2Mult1-b: $x * 1L"); x * 1L}).toLink("linkInc2Mult1-b")

  val linkMult2Dec1: Link[Int, Int] =
    ((x: Int) => {logger.debug(s"linkMult2Dec1-a: $x * 2"); x * 2}).toLink("linkMult2Dec1-a") ~>
    ((x: Int) => {logger.debug(s"linkMult2Dec1-b: $x - 1"); x - 1}).toLink("linkMult2Dec1-b")

  val linkI2S: Link[Int, String] =
    ((x: Int) => {logger.debug(s"linkI2S-a: $x.toString"); x.toString}).toLink("linkI2S-a")

  val linkInc1: Link[Long, Long] =
    ((x: Long) => {logger.debug(s"linkInc1-a: $x + 1L"); x + 1L}).toLink("linkInc1-a")

  implicit def intToString(x: Int) = x.toString
  implicit def longToInt(x: Long) = x.toInt
}
