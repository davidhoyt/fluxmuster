package com.github.davidhoyt.fluxmuster

class TypeTagTreeSpec extends UnitSpec {
  import scala.reflect.runtime.universe._
  import scala.language.higherKinds

  def withTypeTag[A : TypeTag]: TypeTag[A] =
    implicitly[TypeTag[A]]

  def withTypeTagHigherKinded[A[_]](implicit tA: TypeTag[A[_]]): TypeTag[A[_]] =
    tA

  def withTypeTagTree[A : TypeTagTree]: TypeTagTree[A] =
    implicitly[TypeTagTree[A]]

  def withTypeTagTree[A : TypeTagTree, B : TypeTagTree](tuple: (A, B)): (TypeTagTree[A], TypeTagTree[B]) =
    (implicitly[TypeTagTree[A]], implicitly[TypeTagTree[B]])

  def withTypeTagTreeHigherKinded[A[_]](implicit tA: TypeTagTree[A[_]]): TypeTagTree[A[_]] =
    tA

  case class MultiParameterCaseClass2[A, B](a: A, b: B)(implicit val tA: TypeTagTree[A], val tB: TypeTagTree[B])
  case class MultiParameterCaseClass3[A, B, C](a: A, b: B, c: C)(implicit val tA: TypeTagTree[A], val tB: TypeTagTree[B], val tC: TypeTagTree[C])
  type AliasForCaseClassWithAlternateParameterCount2[A] = MultiParameterCaseClass2[A, A]
  type AliasForCaseClassWithAlternateParameterCount3[A] = MultiParameterCaseClass3[A, A, A]
  type Structural = { def methodA(a: Int): Unit ; def methodB: Int }


  behavior of "TypeTagTree"

  it should "construct valid trees for simple types" in {
    val a = MultiParameterCaseClass3("<string>", 0, 0.0f)
    a.tA.toShortString should be("String")
    a.tB.toShortString should be("Int")
    a.tC.toShortString should be("Float")
  }

  it should "construct valid trees for moderately complex types" in {
    val a = MultiParameterCaseClass2(Seq(Seq(0)), Seq(1))
    a.tA.toShortString should be("Seq[Seq[Int]]")
    a.tB.toShortString should be("Seq[Int]")
  }

  it should "construct valid trees for tuples" in {
    //Tuple
    val (tA, tB) = withTypeTagTree((MultiParameterCaseClass2(Seq(Vector("")), ""), (0L, 0)))
    tA.toShortString should be("MultiParameterCaseClass2[Seq[Vector[String]], String]")

    //For tuples, it should use () instead of TupleN[...].
    tB.toShortString should be("(Long, Int)")
  }

  it should "construct valid trees for structural types" in {
    //Structural type
    val a = withTypeTagTree[Structural]
    a.toShortString should be("AnyRef{def methodA(a: Int): Unit; def methodB: Int}")
  }

  it should "construct valid trees for types with existential types" in {
    //Covariant
    val a = withTypeTagTree[(Vector[Seq[_]], List[_])]
    a.toShortString should be("(Vector[Seq[_]], List[_])")

    //Invariant
    val b = withTypeTagTree[Set[_]]
    b.toShortString should be ("Set[_]")

    //Multiple type parameters
    val c = withTypeTagTree[MultiParameterCaseClass3[_, _, Int]]
    c.toShortString should be ("MultiParameterCaseClass3[_, _, Int]")
  }

  it should "construct valid trees for higher-kinded types" in {
    //Higher-kinded
    val a = withTypeTagTreeHigherKinded[Seq]
    a.toShortString should be ("Seq[_]")
//    val a = withTypeTagHigherKinded[Seq]
//    a.toString() should be ("Seq[_]")
  }

  it should "construct valid trees for functions" in {

  }
}
