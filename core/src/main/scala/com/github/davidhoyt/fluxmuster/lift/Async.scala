package com.github.davidhoyt.fluxmuster.lift

import com.github.davidhoyt.fluxmuster._
import scala.concurrent.{ExecutionContext, Future}

object Async extends LiftFactory[ExecutionContext, Future] {

  import scala.language.higherKinds

  val defaultName =
    Macros.simpleNameOf[Async.type]

  protected val ops =
    FutureLiftOps
}
