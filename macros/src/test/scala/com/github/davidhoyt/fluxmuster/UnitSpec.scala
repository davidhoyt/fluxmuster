package com.github.davidhoyt.fluxmuster

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

trait UnitSpec extends FlatSpecLike with Matchers with OptionValues with Inside with Inspectors with ScalaFutures
