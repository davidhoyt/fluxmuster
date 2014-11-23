package com.github.davidhoyt.fluxmuster

trait Named {
  val name: String
}

class ListArray[+T](private[this] val existing: Array[T]) {
  import scala.reflect.ClassTag

  def array[U >: T]: Array[U] = existing.asInstanceOf[Array[U]]
  def size: Int = existing.length
  def length: Int = existing.length
  def foldLeft[A](initial: A)(fn: (A, T) => A): A = array.foldLeft(initial)(fn)
  def ++[U >: T : ClassTag](other: ListArray[U]): ListArray[U] = new ListArray(existing ++: other.array)
  def +:[U >: T : ClassTag](addl: U) = new ListArray(addl +: array)
}

object ListArray {
  import scala.reflect.ClassTag

  def apply[T : ClassTag](from: T*): ListArray[T] = new ListArray(from.toArray)
  def apply[T](from: Array[T]): ListArray[T] = new ListArray(from)
}

trait Link[-In, +Out] extends Named {
  protected def process[A, B](a: A)(implicit aToIn: A => In, outToB: Out => B): B

  val sequence: ListArray[LinkAny] = ListArray(Link.this)

  protected[this] val typeIn: TypeTagTree[In]
  protected[this] val typeOut: TypeTagTree[Out]


  implicit def in[In0 <: In]: TypeTagTree[In0] = typeIn.asInstanceOf[TypeTagTree[In0]]
  implicit def out[Out0 >: Out]: TypeTagTree[Out0] = typeOut.asInstanceOf[TypeTagTree[Out0]]

  protected[this] def asShortString: String = null

  lazy val asDefaultString = s"$name"
  override def toString = toShortString

  lazy val toShortString = {
    val short = asShortString
    if (short eq null)
      asDefaultString
    else
      short
  }

  override def hashCode: Int =
    (typeIn.hashCode() * 31) +
    typeOut.hashCode()

  override def equals(other: Any): Boolean =
    other match {
      case ref: AnyRef =>
        ref eq Link.this
      case _ =>
        false
    }

  def apply[In0, Out0](a: In0)(implicit aToIn: In0 => In, outToB: Out => Out0): Out0 = {
    val out = process(a)(aToIn, identity)
    outToB(out)
  }

  def run(in: In): Out = apply(in)(identity, identity)
  implicit val toFunction: In => Out = run

  def asFunction[In0, Out0](implicit aToIn: In0 => In, outToB: Out => Out0): In0 => Out0 =
    (a: In0) => apply(aToIn(a))(identity, outToB)

  def andThen[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Link[In, OtherOut] = {
    val ref = Link.this
    new Link[In, OtherOut] {
      val name = s"${ref.name} ~> ${other.name}"
      override val sequence = ref.sequence ++ other.sequence
      override protected[this] val typeIn: TypeTagTree[In] = ref.in
      override protected[this] val typeOut: TypeTagTree[OtherOut] = other.out
      override protected def process[A, B](a: A)(implicit aToIn: (A) => In, outToB: (OtherOut) => B): B =
        other.apply(ref.apply(aToIn(a))(identity, thisOutToOtherIn))
    }
  }

  def compose[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Link[OtherIn, Out] = {
    val ref = Link.this
    new Link[OtherIn, Out] {
      val name = s"${other.name} ~> ${ref.name}"
      override val sequence = other.sequence ++ ref.sequence
      override protected[this] val typeIn: TypeTagTree[OtherIn] = other.in
      override protected[this] val typeOut: TypeTagTree[Out] = ref.out
      override protected def process[A, B](a: A)(implicit aToIn: A => OtherIn, outToB: Out => B): B =
        ref.apply(other.apply(aToIn(a))(identity, otherOutToThisIn))
    }
  }

  import scala.collection.generic.CanBuildFrom
  import scala.concurrent.{Await, Awaitable, Future, ExecutionContext}
  import scala.concurrent.duration.Duration
  import scala.language.higherKinds

  def tee[OtherIn, OtherOut, M[X] <: Traversable[X]](other: M[Link[OtherIn, OtherOut]])(implicit context: ExecutionContext, thisOutToOtherIn: Out => OtherIn, cbf: CanBuildFrom[M[OtherOut], OtherOut, M[OtherOut]], typeIn: TypeTagTree[OtherIn], typeOut: TypeTagTree[Future[M[OtherOut]]]): Link[In, Future[M[OtherOut]]] =
    andThen(Implicits.functionToLink("tee") { next: OtherIn =>
      val results =
        for {
          l <- other
        } yield Future(l.run(next))

      Future.sequence(results) map (x => (cbf() ++= x).result())
    })

  def await[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit atMost: Duration, evidence: Out => Awaitable[OtherIn]): Link[In, OtherOut] =
    andThen(other)(x => Await.result(evidence(x), atMost))


  def ~>[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit thisOutToOtherIn: Out => OtherIn): Link[In, OtherOut] =
    andThen(other)

  def <~[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit otherOutToThisIn: OtherOut => In): Link[OtherIn, Out] =
    compose(other)

  def #>[OtherIn, OtherOut](other: Link[OtherIn, OtherOut]*)(implicit context: ExecutionContext, thisOutToOtherIn: Out => OtherIn, typeIn: TypeTagTree[OtherIn], typeOut: TypeTagTree[Future[Seq[OtherOut]]]): Link[In, Future[Seq[OtherOut]]] =
    tee(other)

  def tee[OtherIn, OtherOut](other: Link[OtherIn, OtherOut]*)(implicit context: ExecutionContext, thisOutToOtherIn: Out => OtherIn, typeIn: TypeTagTree[OtherIn], typeOut: TypeTagTree[Future[Seq[OtherOut]]]): Link[In, Future[Seq[OtherOut]]] =
    tee(other)

  def #>[OtherIn, OtherOut, M[X] <: Traversable[X]](other: M[Link[OtherIn, OtherOut]])(implicit context: ExecutionContext, thisOutToOtherIn: Out => OtherIn, cbf: CanBuildFrom[M[OtherOut], OtherOut, M[OtherOut]], typeIn: TypeTagTree[OtherIn], typeOut: TypeTagTree[Future[M[OtherOut]]]): Link[In, Future[M[OtherOut]]] =
    tee(other)

  def %>[OtherIn, OtherOut](other: Link[OtherIn, OtherOut])(implicit atMost: Duration, evidence: Out => Awaitable[OtherIn]): Link[In, OtherOut] =
    await(other)
}

object Link {

}
