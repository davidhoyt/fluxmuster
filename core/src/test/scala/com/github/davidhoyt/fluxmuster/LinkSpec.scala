package com.github.davidhoyt.fluxmuster

import scala.annotation.unchecked.uncheckedVariance

trait Foo[-A] {
  //@uncheckedVariance val a: A

}
trait A
class B extends A
class C extends A

class LinkSpec extends UnitSpec {
  import Links._

  behavior of Macros.simpleNameOf[Link.type]

  it should "correctly tee values" in {
    linkS2Sum.run("0") should be (3L)
  }
}