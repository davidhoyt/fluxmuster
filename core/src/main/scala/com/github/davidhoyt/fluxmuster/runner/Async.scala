package com.github.davidhoyt.fluxmuster.runner

import com.github.davidhoyt.fluxmuster._
import scala.concurrent.{ExecutionContext, Future}

object Async extends RunnerFactory[ExecutionContext, Future] {

  import scala.language.higherKinds

  val defaultName =
    Macros.simpleNameOf[Async.type]

  protected val ops =
    FutureRunnerOps
}
