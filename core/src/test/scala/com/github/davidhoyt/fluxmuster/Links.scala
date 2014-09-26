package com.github.davidhoyt.fluxmuster

object Links {
  import com.typesafe.scalalogging.Logger
  import org.slf4j.LoggerFactory
  import Implicits._

  import scala.language.implicitConversions

  private val logger = Logger(LoggerFactory.getLogger(Macros.simpleNameOf[Links.type]))

  val linkS2L: Link[String, Long] =
    ((x: String) => {logger.debug(s"linkS2L: '$x'.toLong"); x.toLong}).toLink("linkS2L")

  val linkInc2 =
    ((x: Long) => {logger.debug(s"linkInc2: $x + 2L"); x + 2L}).toLink("linkInc2")

  val linkMult1 =
    ((x: Long) => {logger.debug(s"linkMult1: $x * 1L"); x * 1L}).toLink("linkMult1")

  val linkInc2Mult1: Link[Long, Long] =
    linkInc2 ~> linkMult1

  val linkMult2 =
    ((x: Int) => {logger.debug(s"linkMult2: $x * 2"); x * 2}).toLink("linkMult2")

  val linkDec1 =
    ((x: Int) => {logger.debug(s"linkDec1: $x - 1"); x - 1}).toLink("linkDec1")

  val linkMult2Dec1: Link[Int, Int] =
    linkMult2 ~> linkDec1

  val linkI2S: Link[Int, String] =
    ((x: Int) => {logger.debug(s"linkI2S: $x.toString"); x.toString}).toLink("linkI2S")

  val linkInc1: Link[Long, Long] =
    ((x: Long) => {logger.debug(s"linkInc1: $x + 1L"); x + 1L}).toLink("linkInc1")

  implicit def intToString(x: Int) = x.toString
  implicit def longToInt(x: Long) = x.toInt
}
