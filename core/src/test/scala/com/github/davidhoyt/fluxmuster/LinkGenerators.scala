package com.github.davidhoyt.fluxmuster

import scala.util.Random

object LinkGenerators {
  import org.scalacheck.{Arbitrary, Gen}
  import Implicits._

  import scala.language.existentials

  case class GeneratedLink(default: Any, link: Link[Any, Any])
  case class Links(default: Any, links: Vector[Link[_, _]])

  object Types {
    val Int    = Seq(typeTagTreeOf[Int], typeTagTreeOf[java.lang.Integer])
    val Long   = Seq(typeTagTreeOf[Long], typeTagTreeOf[java.lang.Long])
    val String = Seq(typeTagTreeOf[String], typeTagTreeOf[java.lang.String])
    val Double = Seq(typeTagTreeOf[Double], typeTagTreeOf[java.lang.Double])
  }

  val functions = Map[Seq[TypeTagTree[_]], Links](
    Types.Int ->
      Links(0, Vector(
        (identity[Int] _).toLink("identity"),
        (Functions.int2String _).toLink("int2String"),
        (Functions.int2Long _).toLink("int2Long"),
        (Functions.int2Double _).toLink("int2Double")
      )),
    Types.Long ->
      Links(0L, Vector(
        (identity[Long] _).toLink("identity"),
        (Functions.long2String _).toLink("long2String"),
        (Functions.long2Int _).toLink("long2Int"),
        (Functions.long2Double _).toLink("long2Double")
      )),
    Types.Double ->
      Links(0.0D, Vector(
        (identity[Double] _).toLink("identity"),
        (Functions.double2Int _).toLink("double2Int"),
        (Functions.double2Long _).toLink("double2Long")
      )),
    Types.String ->
      Links("0", Vector(
        (identity[String] _).toLink("identity"),
        (Functions.string2Int _).toLink("string2Int"),
        (Functions.string2Long _).toLink("string2Long")
      ))
  ).toIndexedSeq

  object Functions {
    def int2Long(x: Int) = x.toLong
    def int2Double(x: Int) = x.toDouble
    def int2String(x: Int) = x.toString
    def long2Int(x: Long) = x.toInt
    def long2Double(x: Long) = x.toDouble
    def long2String(x: Long) = x.toString
    def double2Int(x: Double) = x.toInt
    def double2Long(x: Double) = x.toLong
    def double2String(x: Double) = x.toString
    def string2Int(x: String) = x.toInt
    def string2Long(x: String) = x.toLong
    def string2Double(x: String) = x.toDouble
  }

  def randomType: (Any, TypeTagTree[_]) = {
    val idx = Random.nextInt(functions.size)
    val (t, Links(default, _)) = functions(idx)
    (default, t.head)
  }

  def randomLink(key: TypeTagTree[_]): Link[Any, Any] = {
    val links = functions.collectFirst {
      case (typesFromFuncs, Links(_, found)) if typesFromFuncs contains key =>
        found
    }.get
    val idx = Random.nextInt(links.size)
    links(idx).asInstanceOf[Link[Any, Any]]
  }

  def combineRandomLink(link: Link[Any, Any]): Link[Any, Any] =
    link ~> randomLink(link.out)

  def genLink(maxChainSize: Int = 100): Gen[GeneratedLink] =
    for {
      iNumberLinks <- Gen.choose(1, maxChainSize)
      (startDefault, startType) = randomType
      startLink = randomLink(startType)
    } yield {
      val resultLink =
        (0 to iNumberLinks).foldLeft(startLink) {
          case (link, _) =>
            combineRandomLink(link)
        }
      GeneratedLink(startDefault, resultLink)
    }

  implicit val arbitraryGeneratedLink =
    Arbitrary[GeneratedLink](genLink())
}
