package com.github.davidhoyt.fluxmuster

class TypeTagTreeSpec extends UnitSpec {
  import scala.reflect.runtime.universe._
  import scala.language.higherKinds

  def withTypeTag[A : TypeTag]: TypeTag[A] =
    implicitly[TypeTag[A]]

  def withTypeTag[A : TypeTag](a: A): TypeTag[A] =
    implicitly[TypeTag[A]]

  def withTypeTagHigherKinded[A[_]](implicit tA: TypeTag[A[_]]): TypeTag[A[_]] =
    tA

  def withTypeTagTree[A : TypeTagTree]: TypeTagTree[A] =
    implicitly[TypeTagTree[A]]

  def withTypeTagTree[A : TypeTagTree](a: A): TypeTagTree[A] =
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
  class Path {
    val d = new Dependent
    class Dependent {
      val t = new Type
      class Type
    }
  }
  def newPathDependentType: Path#Dependent#Type = {
    val p = new Path
    val d = new p.Dependent
    new d.Type()
  }
  trait SingleParameterTraitContravariant[-A]
  trait SingleParameterTraitInvariant[A]
  trait SingleParameterTraitCovariant[+A]


  behavior of "TypeTagTree"

  it should "construct valid trees for simple types" in {
    val a = MultiParameterCaseClass3("<string>", 0, 0.0f)
    a.tA.toShortString should be("java.lang.String")
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
    tA.toShortString should be("TypeTagTreeSpec.this.MultiParameterCaseClass2[Seq[scala.collection.immutable.Vector[java.lang.String]],java.lang.String]")

    //For tuples, it should use () instead of TupleN[...].
    tB.toShortString should be("(Long, Int)")
  }

  it should "construct valid trees for structural types" in {
    //Structural type
    val a = withTypeTagTree[Structural]
    a.toShortString should be ("TypeTagTreeSpec.this.Structural")

    val b = withTypeTagTree[{def methodA(a: Int): Unit; def methodB: Int}]
    b.toShortString should be("AnyRef{def methodA(a: Int): Unit; def methodB: Int}")
  }

  it should "construct valid trees for existential types" in {
    //Covariant
    val a = withTypeTagTree[(Vector[Seq[_]], List[_], MultiParameterCaseClass3[_, _, _])]
    a.toShortString should be("(Vector[Seq[_]], List[_], TypeTagTreeSpec.this.MultiParameterCaseClass3[_, _, _])")
    //TODO: Why doesn't equality work for existential types?
    //a.typeParameters.types should be (Seq(typeOf[Vector[Seq[_]]], typeOf[List[_]], typeOf[MultiParameterCaseClass3[_, _, _]]))

    //Invariant
    val b = withTypeTagTree[Set[_]]
    b.toShortString should be ("Set[_]")

    //Multiple type parameters
    val c = withTypeTagTree[MultiParameterCaseClass3[_, _, Int]]
    c.toShortString should be ("TypeTagTreeSpec.this.MultiParameterCaseClass3[_, _, Int]")
  }

  it should "construct valid trees for functions" in {
    val a = withTypeTagTree[String => Int]
    a.toShortString should be ("String => Int")
    a.typeParameters.types should be (Seq(typeOf[String], typeOf[Int]))

    val b = withTypeTagTree[(String, String) => Int]
    b.toShortString should be ("(String, String) => Int")
    b.typeParameters.types should be (Seq(typeOf[String], typeOf[String], typeOf[Int]))

    val c = withTypeTagTree[((String, String)) => Int]
    c.toShortString should be ("((String, String)) => Int")
    c.typeParameters.types should be (Seq(typeOf[(String, String)], typeOf[Int]))
  }

  //it should "construct valid trees for higher-kinded types" in {
    //Higher-kinded
    //val a = withTypeTagTreeHigherKinded[Seq]
    //a.toShortString should be ("Seq[_]")
    ////val b = withTypeTagHigherKinded[Seq]
    ////b.toString() should be ("Seq[_]")
  //}

  it should "construct valid trees for anonymous classes" in {
    //val a0 = withTypeTag(new SingleParameterTraitContravariant[String] {})
    //a0.toString should be ("TypeTagTreeSpec.this.SingleParameterTraitContravariant[String]")

    val a = withTypeTagTree(new SingleParameterTraitContravariant[String] with SingleParameterTraitInvariant[Long] {})
    a.toShortString should be ("TypeTagTreeSpec.this.SingleParameterTraitContravariant[String] with TypeTagTreeSpec.this.SingleParameterTraitInvariant[Long]")
    a.typeParameters.head.typeParameters.types should be (Seq(typeOf[String]))
    a.typeParameters.types should be (Seq(typeOf[SingleParameterTraitContravariant[String]], typeOf[SingleParameterTraitInvariant[Long]]))
  }

  it should "construct valid trees for refined types" in {
    val a = withTypeTagTree[SingleParameterTraitInvariant[Double] with SingleParameterTraitContravariant[SingleParameterTraitInvariant[Long]]]
    a.toShortString should be ("TypeTagTreeSpec.this.SingleParameterTraitInvariant[Double] with TypeTagTreeSpec.this.SingleParameterTraitContravariant[TypeTagTreeSpec.this.SingleParameterTraitInvariant[Long]]")
    a.typeParameters.types should be (Seq(typeOf[SingleParameterTraitInvariant[Double]], typeOf[SingleParameterTraitContravariant[SingleParameterTraitInvariant[Long]]]))
  }

  it should "construct valid trees for path dependent types" in {
    val a = withTypeTagTree[Path#Dependent#Type]
    a.toShortString should be ("TypeTagTreeSpec.this.Path#Dependent#Type")

    val b = withTypeTagTree(newPathDependentType)
    b.toShortString should be ("TypeTagTreeSpec.this.Path#Dependent#Type")

    val c = withTypeTagTree((new Path).d.t)
    c.toShortString should be ("TypeTagTreeSpec.this.Path#Dependent#Type")

    //val d = withTypeTag((new Path).d.t)
    //d.toString() should be ("Type")
  }

  it should "construct valid trees for deeply nested complex types" in {
    val a = withTypeTagTree[MultiParameterCaseClass3[MultiParameterCaseClass2[MultiParameterCaseClass2[AliasForCaseClassWithAlternateParameterCount2[String], Structural], String], Vector[Seq[Array[Seq[Vector[Seq[Seq[Seq[Seq[Array[SingleParameterTraitContravariant[MultiParameterCaseClass2[Seq[Vector[Array[String]]], java.util.ArrayList[java.util.HashSet[Vector[String]]]]]]]]]]]]]]], MultiParameterCaseClass2[SingleParameterTraitInvariant[String], MultiParameterCaseClass3[Seq[Array[java.util.Map[String, Long]]], Int, String => (Int, (Long, Int, Seq[String] => List[::[Int]]))]]]]
    a.toShortString should be ("TypeTagTreeSpec.this.MultiParameterCaseClass3[TypeTagTreeSpec.this.MultiParameterCaseClass2[TypeTagTreeSpec.this.MultiParameterCaseClass2[TypeTagTreeSpec.this.AliasForCaseClassWithAlternateParameterCount2[String],TypeTagTreeSpec.this.Structural],String],Vector[Seq[Array[Seq[Vector[Seq[Seq[Seq[Seq[Array[TypeTagTreeSpec.this.SingleParameterTraitContravariant[TypeTagTreeSpec.this.MultiParameterCaseClass2[Seq[Vector[Array[String]]],java.util.ArrayList[java.util.HashSet[Vector[String]]]]]]]]]]]]]]],TypeTagTreeSpec.this.MultiParameterCaseClass2[TypeTagTreeSpec.this.SingleParameterTraitInvariant[String],TypeTagTreeSpec.this.MultiParameterCaseClass3[Seq[Array[java.util.Map[String,Long]]],Int,String => (Int, (Long, Int, Seq[String] => List[::[Int]]))]]]")
    a.typeParameters.types should be (Seq(typeOf[ TypeTagTreeSpec.this.MultiParameterCaseClass2[TypeTagTreeSpec.this.MultiParameterCaseClass2[TypeTagTreeSpec.this.AliasForCaseClassWithAlternateParameterCount2[String],TypeTagTreeSpec.this.Structural],String] ], typeOf[ Vector[Seq[Array[Seq[Vector[Seq[Seq[Seq[Seq[Array[TypeTagTreeSpec.this.SingleParameterTraitContravariant[TypeTagTreeSpec.this.MultiParameterCaseClass2[Seq[Vector[Array[String]]],java.util.ArrayList[java.util.HashSet[Vector[String]]]]]]]]]]]]]]] ], typeOf[ TypeTagTreeSpec.this.MultiParameterCaseClass2[TypeTagTreeSpec.this.SingleParameterTraitInvariant[String],TypeTagTreeSpec.this.MultiParameterCaseClass3[Seq[Array[java.util.Map[String,Long]]], Int, String => (Int, (Long, Int, Seq[String] => List[::[Int]]))]] ]))
  }

}
