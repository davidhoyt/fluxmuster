package com.github.davidhoyt.fluxmuster3

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

object StepDsl {
  type Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] with StepChaining with Named
}

trait StepDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  self: StepDsl.Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =>

  def ~>[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A, proofDownstreamCanRouteToUpstream: B => UpstreamIn): Step[DownstreamIn, B, UpstreamIn, UpstreamOut] =
    down(linked)

  def down[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A, proofDownstreamCanRouteToUpstream: B => UpstreamIn): Step[DownstreamIn, B, UpstreamIn, UpstreamOut] = {
    //implicit val foo = linked.
    Step.create(name)(downstream.andThen(linked)(proofDownstreamOutCanRouteToLinkedIn))(upstream)
  }

  def <~[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A, proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): Step[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    up(linked)

  def up[A, B](linked: Link[A, B])(implicit proofDownstreamOutCanRouteToLinkedIn: DownstreamOut => A, proofLinkedOutCanRouteToUpstreamIn: B => UpstreamIn): Step[DownstreamIn, DownstreamOut, A, UpstreamOut] =
    Step.create(name)(downstream)(upstream.compose(linked)(proofLinkedOutCanRouteToUpstreamIn))

  def combine[A, B, C, D](other: Step[A, B, C, D])(implicit connect: DownstreamOut => A, connect2: D => UpstreamIn): Step[DownstreamIn, B, C, UpstreamOut] = {
    implicit val bToC = other.proofDownstreamCanRouteToUpstream
    Step.createCombined(chain, other.chain)(downstream andThen other.downstream)(upstream compose other.upstream)
  }
}

object StepRun {
  type Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =
    StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
}

trait StepRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  //extends ChainRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] {
  self: StepRun.Dependencies[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut] =>

  val proofDownstreamCanRouteToUpstream: DownstreamOut => UpstreamIn

  def apply[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit addlProofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    run(e)

  def run[E, F >: DownstreamOut, G <: UpstreamIn](e: E)(implicit addlProofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn): UpstreamOut =
    upstream(addlProofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity)))

  /** Generates a [[Link]] instance that when called runs the downstream and then passes it to the upstream. */
  implicit def toLink: Link[DownstreamIn, UpstreamOut] =
    createLink[DownstreamIn, DownstreamOut, UpstreamIn](proofDownstreamCanRouteToUpstream, identity, downstream.typeIn)

  /** Generates a [[Link]] instance that when called runs the downstream and then passes it to the upstream. */
  implicit def createLink[E, F >: DownstreamOut, G <: UpstreamIn](implicit addlProofDownstreamCanRouteToUpstream: F => G, mapToIn: E => DownstreamIn, tE: TypeTagTree[E]): Link[E, UpstreamOut] =
    Link((e: E) => upstream(addlProofDownstreamCanRouteToUpstream(downstream.apply(e)(mapToIn, identity))))(tE, upstream.typeOut)

//  def routeDownToUp(in: DownstreamOut): UpstreamIn =
//    proofDownstreamCanRouteToUpstream(in)
//
//  def runDownChain(in: DownstreamIn): DownstreamOut =
//    downstream(in)
//
//  def runUpChain(in: UpstreamIn): UpstreamOut =
//    upstream(in)
}

trait Step[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  extends StepLike[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with StepDsl[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with StepRun[DownstreamIn, DownstreamOut, UpstreamIn, UpstreamOut]
  with StepChaining {
  self: Named =>

}

object EmptyStep {
  val NAME = Macros.simpleNameOf[EmptyStep.type]

  private case class Build(name: String) extends EmptyStep {
    override def toString =
      s"$NAME($name)"
  }

  def apply(name: String): EmptyStep =
    Build(name)
}

trait EmptyStep extends Named {
  def #>[A, B](linked: Link[A, B]): DownstreamStep[A, B] =
    down(linked)
    //Step.create[A, B, B, B](name)(linked)(Link.identity[B](linked.typeOut))

  def #>[A, B](downStep: DownstreamStep[A, B]): DownstreamStep[A, B] =
    downStep

  def #>[A, B, C, D](step: Step[A, B, C, D]): Step[A, B, C, D] =
    step

  def <#[A, B](linked: Link[A, B]): UpstreamStep[A, B] =
    up(linked)
    //Step.create[A, A, A, B](name)(Link.identity[A](linked.typeIn))(linked)

  def <#[A, B](upStep: UpstreamStep[A, B]): UpstreamStep[A, B] =
    upStep

  def <#[A, B, C, D](step: Step[A, B, C, D]): Step[A, B, C, D] =
    step

  def down[A, B](linked: Link[A, B]): DownstreamStep[A, B] =
    DownstreamStep(name)(linked)

  def up[A, B](linked: Link[A, B]): UpstreamStep[A, B] =
    UpstreamStep(name)(linked)
}

trait DownstreamStep[In, Out] extends Named {
  val link: Link[In, Out]

  def ~>[A, B](linked: Link[A, B])(implicit connect: Out => A): DownstreamStep[In, B] =
    DownstreamStep(name)(link andThen linked)

  def <#[A, B](linked: Link[A, B])(implicit connect: Out => A): Step[In, Out, A, B] =
    Step.create(name)(link)(linked)

  def <#[A, B](upStep: UpstreamStep[A, B])(implicit connect: Out => A): Step[In, Out, A, B] =
    Step.create(name)(link)(upStep.link)
}

object DownstreamStep {
  val NAME = Macros.simpleNameOf[DownstreamStep.type]

  private case class Build[In, Out](name: String, link: Link[In, Out]) extends DownstreamStep[In, Out] with Named {
    override def toString =
      s"$NAME($name)[${link.typeIn.toShortString}, ${link.typeOut.toShortString}]"
  }

  def apply[In, Out](name: String)(link: Link[In, Out]): DownstreamStep[In, Out] =
    Build(name, link)
}

trait UpstreamStep[In, Out] extends Named {
  val link: Link[In, Out]

  def <~[A, B](linked: Link[A, B])(implicit connect: B => In): UpstreamStep[A, Out] =
    UpstreamStep(name)(link compose linked)

  def #>[A, B](linked: Link[A, B])(implicit connect: B => In): Step[A, B, In, Out] =
    Step.create(name)(linked)(link)

  def #>[A, B](downStep: DownstreamStep[A, B])(implicit connect: B => In): Step[A, B, In, Out] =
    Step.create(name)(downStep.link)(link)
}

object UpstreamStep {
  val NAME = Macros.simpleNameOf[UpstreamStep.type]

  private case class Build[In, Out](name: String, link: Link[In, Out]) extends UpstreamStep[In, Out] with Named {
    override def toString =
      s"$NAME($name)[${link.typeIn.toShortString}, ${link.typeOut.toShortString}]"
  }

  def apply[In, Out](name: String)(link: Link[In, Out]): UpstreamStep[In, Out] =
    Build(name, link)
}

object Step {
  val NAME = s"${Macros.simpleNameOf[Step.type]}"

  private case class Build[A, B, C, D](name: String, downstream: Downstream[A, B], upstream: Upstream[C, D], mine: ChainStep, other: ChainStep, proofDownstreamCanRouteToUpstream: B => C)(val chaining: FnChainStep) extends Step[A, B, C, D] with Named {
    lazy val chain =
      chainTogether(this, mine, other)

    def chainTogether(instance: ChainableStep, mine: ChainStep, other: ChainStep): ChainStep =
      chaining(instance, mine, other)

    override def toString =
      s"$NAME($name)[${downstream.typeIn.toShortString}, ${downstream.typeOut.toShortString}, ${upstream.typeIn.toShortString}, ${upstream.typeOut.toShortString}]"
  }

  def #>[A, B](linked: Link[A, B]): DownstreamStep[A, B] =
    DownstreamStep(NAME)(linked)
    //create[A, B, B, B](linked)(Link.identity[B](linked.typeOut))

  def <#[A, B](linked: Link[A, B]): UpstreamStep[A, B] =
    UpstreamStep(NAME)(linked)
    //create[A, A, A, B](Link.identity[A](linked.typeIn))(linked)

  def create[A, B, C, D](downstream: Downstream[A, B])(upstream: Upstream[C, D])(implicit downToUp: B => C): Step[A, B, C, D] =
    create(Macros.simpleNameOf[Step.type])(downstream)(upstream)

  def create[A, B, C, D](name: String)(downstream: Downstream[A, B])(upstream: Upstream[C, D])(implicit downToUp: B => C): Step[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream, EmptyChainStep, EmptyChainStep, downToUp)(StepProvidedChain.apply)

  def createCombined[A, B, C, D](mine: ChainStep, other: ChainStep)(downstream: Downstream[A, B])(upstream: Upstream[C, D])(implicit downToUp: B => C): Step[A, B, C, D] =
    createCombined(Macros.simpleNameOf[Step.type])(mine, other)(downstream)(upstream)(downToUp)

  def createCombined[A, B, C, D](name: String)(mine: ChainStep, other: ChainStep)(downstream: Downstream[A, B])(upstream: Upstream[C, D])(implicit downToUp: B => C): Step[A, B, C, D] =
    Build[A, B, C, D](name, downstream, upstream, mine, other, downToUp)(StepCombinedChain.apply)

  def apply(name: String): EmptyStep =
    EmptyStep(name)

  /** Converts a function into a [[Step]] instance where the function is evaluated in the downstream phase. */
  def downstream[A, B](fn: A => B)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): Step[A, B, B, B] = {
    val link = Link(fn)(tA, tB)
    create(fn.toString())(link)(Link.identity[B])
  }

  /** Converts a function into a [[Step]] instance where the function is evaluated in the upstream phase. */
  def upstream[A, B](fn: A => B)(implicit tA: TypeTagTree[A], tB: TypeTagTree[B]): Step[A, A, A, B] = {
    val link = Link(fn)(tA, tB)
    create(fn.toString())(Link.identity[A])(link)
  }

  def apply[A, B, C, D](downstream: A => B)(upstream: C => D)(implicit proofDownstreamCanMapToUpstream: B => C, tA: TypeTagTree[A], tB: TypeTagTree[B], tC: TypeTagTree[C], tD: TypeTagTree[D]): Step[A, B, C, D] = {
    val down = Link(downstream)(tA, tB)
    val up = Link(upstream)(tC, tD)
    create(downstream.toString())(down)(up)(proofDownstreamCanMapToUpstream)
  }

  def apply[A, B, C, D](downstream: Link[A, B])(upstream: Link[C, D])(implicit proofDownstreamCanMapToUpstream: B => C): Step[A, B, C, D] = {
    create("<link>")(downstream)(upstream)(proofDownstreamCanMapToUpstream)
  }
}
