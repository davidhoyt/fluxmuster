package com.github.davidhoyt.fluxmuster

class ProxySpecificationSpec extends UnitSpec {
  behavior of "ProxySpecification"

  def stringToLong(x: String) = x.toLong
  def longToInt(x: Long) = x.toInt

  it should "properly compose specification connections" in {
    val part0 = Identity[String, Int]("#0")
    val part1 = Identity[String, Int]("#1")
    val part2 = Downstream[String, Long, Int]("#2")(stringToLong)
    val part3 = Identity[Long, Int]("#3")
    val part4 = BiDirectional[Long, Int, Int, Int]("#4")(longToInt)(identity)
    val part5 = Identity[Int, Int]("#5")

    val connected1 = part1 <~> part2
    connected1.connections should be (Seq(part1, part2))

    val connected2 = connected1 <~> part3
    connected2.connections should be (Seq(part1, part2, part3))

    val connected3 = part1 <~> part2 <~> part3
    connected3.connections should be (Seq(part1, part2, part3))

    val connected4 = connected3 <~> part4
    connected4.connections should be (Seq(part1, part2, part3, part4))

    val connected5 = part1 <~> part2 <~> part3 <~> part4
    connected5.connections should be (Seq(part1, part2, part3, part4))

    val connected6 = part3 <~> part4
    val connected7 = connected1 <~> connected6
    connected7.connections should be (Seq(part1, part2, part3, part4))

    val proxy1 = Proxy(connected7)
    proxy1.connections should be (Seq(part1, part2, part3, part4))

    val proxy2 = Proxy(part5)
    proxy2.connections should be (Seq(part5))

    val proxy3 = Proxy(proxy1 <~> proxy2)
    proxy3.connections should be (Seq(part1, part2, part3, part4, part5))

    val connected8 = part0 <~> proxy3
    connected8.connections should be (Seq(part0, part1, part2, part3, part4, part5))

    val proxy4 = Proxy(connected8)
    proxy4.connections should be (Seq(part0, part1, part2, part3, part4, part5))
  }
}
