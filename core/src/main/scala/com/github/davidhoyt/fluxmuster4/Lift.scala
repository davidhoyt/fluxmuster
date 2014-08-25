package com.github.davidhoyt.fluxmuster4

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

import scala.language.higherKinds

trait LiftOps[State, Into[_]] {
  import scala.language.implicitConversions

  implicit def liftRunner[A, D](chained: ChainLink, runner: A => D)(implicit state: State, typeIn: TypeTagTree[A], typeOut: TypeTagTree[D]): A => Into[D]
  implicit def point[A](given: => A)(implicit state: State): Into[A]
  implicit def flatten[A](given: Into[Into[A]])(implicit state: State): Into[A]
  implicit def map[A, B](given: Into[A])(fn: A => B)(implicit state: State): Into[B]
}

sealed trait LiftLike[State, Into[_]] {
  implicit val typeState: TypeTagTree[State]
  implicit val state: State
  implicit val ops: LiftOps[State, Into]

  def runInThisContext[In, Out, From[_]](chained: Chained[In, From[Out]], otherRunner: In => From[Out])(implicit converter: From -> Into, typeIn: TypeTagTree[In], typeFromOut: TypeTagTree[From[Out]], typeIntoOut: TypeTagTree[Into[Out]]): Link[In, Into[Out]] = {
    val chain = chained.chain
    Link((in: In) => {
      val runOtherInThisContext: In => Into[From[Out]] = ops.liftRunner(chain, otherRunner)(state, typeIn, typeFromOut)
      val resultAfterRunning: Into[From[Out]] = runOtherInThisContext(in)

      //flatMap!
      val mapResultBackIntoThisContext = ops.map(resultAfterRunning)(converter.apply)(state)
      val flattenedBackIntoThisContext: Into[Out] = ops.flatten(mapResultBackIntoThisContext)(state)
      flattenedBackIntoThisContext
    })
  }
}

sealed trait Lift[In, Out, State, Into[_]]
  extends LiftLike[State, Into]
  with Chained[In, Into[Out]]
{
  self: Named =>

  import scala.collection.immutable

  val liftChain: ChainLift

  def apply[A, B](in: A)(implicit convert: A => In): Into[Out] =
    run(convert(in))

  def run(in: In): Into[Out] =
    runner(in)


  //TODO: Change hystrix fallback to option

  /** Adds a link to the chain for lifted execution. */
  def andThen[A, D](other: Link[A, D])(implicit proof: Out => A, tOut: TypeTagTree[Into[D]]): Lift[In, D, State, Into] = {
//    val innerChain = chain.head.chain
//    val newChain = innerChain :+ other
//    val runnerForInnerChain: In => Out = innerChain.head.runner.asInstanceOf[In => Out]
//    val newUnliftedRunner = runnerForInnerChain andThen proof andThen other.run
//    // val newUnliftedRunner = unliftedRunner andThen proof andThen other.run
//    val liftedLink = Link(ops.liftRunner(newChain, newUnliftedRunner)(state, typeIn, other.typeOut))(typeIn, tOut)
//    val lift = Lift.create[In, D, State, Into](name, newChain, liftedLink, newUnliftedRunner, state, ops)(typeState, typeIn, tOut)
//    lift
    val newLink = Link((in: In) => {
      val first: Into[Out] = run(in)
      val second: Into[D] = ops.map(first)(out => other.run(proof(out)))(state)
      second
    })(typeIn, tOut)

    val linkForChain = Link((first: Into[Out]) => {
      val second: Into[D] = ops.map(first)(out => other.run(proof(out)))(state)
      second
    })(typeOut, tOut)

    val liftedChain: ChainLink = chain :+ linkForChain
    val lift = Lift.create[In, D, State, Into](name, liftedChain, newLink, liftChain, state, ops)(typeState, typeIn, tOut)
    lift
  }

  def andThen[A, B, C, D](other: Step[A, B, C, D])(implicit proof: Out => A, tOut: TypeTagTree[Into[D]]): Lift[In, D, State, Into] =
    andThen(other.toLink)

  /** Runs this lift and then the provided lift afterwards. */
  def andThen[A, D, S, F[_]](other: Lift[A, D, S, F])(implicit proof: Out => A, converter: F -> Into, tOut: TypeTagTree[Into[D]]): Lift[In, D, State, Into] = {

    val newLink = Link((in: In) => {
      val first: Into[Out] = run(in)
      val second: Into[F[D]] = ops.map(first)(out => other.run(proof(out)))(state)
      val third: Into[Into[D]] = ops.map(second)(converter.apply)(state)
      val result = ops.flatten(third)
      result
    })(typeIn, tOut)

    //Needs a separate link when building the chain because the type
    //it accepts is different.
    val linkForChain = Link((first: Into[Out]) => {
      val second: Into[F[D]] = ops.map(first)(out => other.run(proof(out)))(state)
      val third: Into[Into[D]] = ops.map(second)(converter.apply)(state)
      val result = ops.flatten(third)
      result
    })(typeOut, tOut)

    val liftedChain: ChainLink = chain :+ linkForChain
    val lift = Lift.create[In, D, State, Into]("|>", liftedChain, newLink, liftChain, state, ops)(typeState, typeIn, tOut)
    lift
  }

  def |>[S, F[_]](other: Lift[In, Out, S, F])(implicit converter: F -> Into, tFofD: TypeTagTree[F[Out]]): Lift[In, Out, State, Into] =
    lift(other)

  def lift[S, F[_]](other: Lift[In, Out, S, F])(implicit converter: F -> Into, tFofD: TypeTagTree[F[Out]]): Lift[In, Out, State, Into] = {
    val liftedChain: ChainLink = immutable.Vector(other)
    val foo = runInThisContext(other, other.runner)(converter, typeIn, tFofD, typeOut)
    Lift.create[In, Out, State, Into](name, liftedChain, foo, other.liftChain, state, ops)(typeState, typeIn, typeOut)
  }

  def asShortString: String =
    null

  val asDefaultString = {
    val in = typeIn.toShortString
    val out = typeOut.toShortString

    s"$name[$in, $out]"
  }

  val toShortString = {
    val short = asShortString
    if (short ne null)
      short
    else
      asDefaultString
  }

  override def toString =
    toShortString
}

object Lift {
  import scala.collection.immutable

  private case class Build[A, D, S, F[_]](name: String, chain: ChainLink, link: Link[A, F[D]], liftChainSoFar: ChainLift, state: S, ops: LiftOps[S, F], override val asShortString: String = null)(implicit val typeState: TypeTagTree[S], val typeIn: TypeTagTree[A], val typeOut: TypeTagTree[F[D]]) extends Lift[A, D, S, F] with Named {
    val runner =
      link.runner

    lazy val liftChain: ChainLift =
      if ((liftChainSoFar eq null) || liftChainSoFar.isEmpty)
        immutable.Vector(this.asInstanceOf[ChainableLift])
      else
        this.asInstanceOf[ChainableLift] +: liftChainSoFar
  }

  private[fluxmuster4] def liftLink[A, D, S, F[_]](chained: Chained[A, D], state: S, ops: LiftOps[S, F])(implicit tOut: TypeTagTree[F[D]]): Link[A, F[D]] = {
    val chain = chained.chain
    val runner = chained.runner
    val liftedRunner = Link(ops.liftRunner[A, D](chain, runner)(state, chained.typeIn, chained.typeOut))(chained.typeIn, tOut)
    liftedRunner
  }

  def create[A, D, S, F[_]](name: String, chain: ChainLink, link: Link[A, F[D]], liftChain: ChainLift, state: S, ops: LiftOps[S, F])(implicit tState: TypeTagTree[S], tIn: TypeTagTree[A], tOut: TypeTagTree[F[D]]): Lift[A, D, S, F] =
    Build[A, D, S, F](name, chain, link, liftChain, state, ops)(tState, tIn, tOut)

  def create[A, D, S, F[_]](name: String, chained: Chained[A, D], liftChain: ChainLift, state: S, ops: LiftOps[S, F])(implicit typeState: TypeTagTree[S], typeIn: TypeTagTree[A], typeOut: TypeTagTree[F[D]]): Lift[A, D, S, F] = {
    val liftedRunner = liftLink(chained, state, ops)
    val liftedChain = immutable.Vector(liftedRunner)
    val lifted = create[A, D, S, F](name, liftedChain, liftedRunner, liftChain, state, ops)
    lifted
  }
}

trait LiftNeedsChained[Out, State, Into[_]]
extends LiftLike[State, Into] {
  this: Named =>

  import scala.collection.immutable

//  def |>[A >: In, D <: Out](other: Link[A, D])(implicit tOut: TypeTagTree[Into[D]]): Lift[A, D, State, Into] =
//    lift(other)

  def lift[A](other: Link[A, Out])(implicit typeOut: TypeTagTree[Out], typeIntoOut: TypeTagTree[Into[Out]]): Lift[A, Out, State, Into] =
    Lift.create(name, other, EmptyChainLift, state, ops)(typeState, other.typeIn, typeIntoOut)

//  def |>[A >: In, B, C, D <: Out](other: Step[A, B, C, D])(implicit tOut: TypeTagTree[Into[D]]): Lift[A, D, State, Into] =
//    lift(other)

  def lift[A, B, C](other: Step[A, B, C, Out])(implicit typeOut: TypeTagTree[Out], typeIntoOut: TypeTagTree[Into[Out]]): Lift[A, Out, State, Into] =
    lift(other.toLink)

  def lift[A, S, F[_]](other: Lift[A, Out, S, F])(implicit converter: F -> Into, typeOut: TypeTagTree[Into[Out]], typeFofOut: TypeTagTree[F[Out]], typeIntoFofOut: TypeTagTree[Into[F[Out]]]): Lift[A, Out, State, Into] = {
    val liftedChain = immutable.Vector(other)
    val liftedLink = runInThisContext(other, other.run)(converter, other.typeIn, other.typeOut, typeOut)
    val lift = Lift.create(name, liftedChain, liftedLink, other.liftChain, state, ops)(typeState, other.typeIn, typeOut)
    lift
  }
}

object LiftNeedsChained {
  private case class Build[D, S, F[_]](name: String, state: S, ops: LiftOps[S, F])(implicit val typeState: TypeTagTree[S]) extends LiftNeedsChained[D, S, F] with Named

  def apply[D, S, F[_]](name: String, state: S, ops: LiftOps[S, F])(implicit typeState: TypeTagTree[S]): LiftNeedsChained[D, S, F] =
    Build[D, S, F](name, state, ops)
}

object Async {
  import scala.concurrent.{ExecutionContext, Future}

  val NAME = Macros.simpleNameOf[Async.type]

  def apply[A, D](chained: Chained[A, D])(implicit context: ExecutionContext, typeState: TypeTagTree[ExecutionContext], typeIn: TypeTagTree[A], typeResult: TypeTagTree[Future[D]]): Lift[A, D, ExecutionContext, Future] =
    Lift.create(NAME, chained, EmptyChainLift, context, FutureLiftOps)
}
