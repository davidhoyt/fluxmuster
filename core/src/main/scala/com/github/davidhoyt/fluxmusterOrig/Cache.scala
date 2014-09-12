package com.github.davidhoyt.fluxmusterOrig

import com.github.davidhoyt.fluxmuster.{Macros, TypeTagTree}

trait CacheOps {
  def get[K, V](key: K): Option[V]
  def put[K, V](key: K, value: V): V
  def getOrPut[K, V](key: K, value: => V): V = {
    val attempt = get(key)
    if (attempt.isDefined)
      attempt.get
    else
      put(key, value)
  }
}

object InMemoryCache {
  //import scala.collection._
  import com.google.common.cache.Cache

  def defaultCache: com.google.common.cache.Cache[AnyRef, AnyRef] =
    com.google.common.cache.CacheBuilder.newBuilder()
      .concurrencyLevel(10)
      .initialCapacity(1024)
      .maximumSize(8192)
      .build[AnyRef, AnyRef]()

  def apply(cache: Cache[AnyRef, AnyRef] = defaultCache): CacheOps =
    new CacheOps {
      //val cache = mutable.HashMap[Any, Any]()

      override def get[K, V](key: K): Option[V] =
        Option(cache.getIfPresent(key)).map(_.asInstanceOf[V])
        //cache.get(key).map(_.asInstanceOf[V])

      override def put[K, V](key: K, value: V): V = {
        cache.put(key.asInstanceOf[AnyRef], value.asInstanceOf[AnyRef])
        //cache.put(key, value)
        value
      }
    }
}

object Cache {
  import KeyValueProcessor.{Downstream, Upstream}
  import scala.collection._

  /** Defines how to extract a key given an instance of `A`. */
  type KeyExtractor[A, K] = A => K
  type Downstream[A, K, V] = ProxyPass[(A, K, Option[V])]
  type Upstream[A, K, V] = ProxyPass[(A, K, V)]
  type Step[A, K, V] = ProxyStep[A, Downstream[A, K, V], Upstream[A, K, V], (A, V)]

  //Provide a way to convey cache eviction, replace, etc.?

  def apply[K, V](implicit ops: CacheOps, tA: TypeTagTree[K], tB: TypeTagTree[Downstream[K, K, V]], tC: TypeTagTree[Upstream[K, K, V]], tD: TypeTagTree[(K, V)]): Step[K, K, V] =
    new CacheStep[K, K, V](identity, ops, tA, tB, tC, tD)

  @scala.annotation.implicitNotFound("Please specify a cache typeclass to use.")
  def apply[A, K, V](keyExtractor: KeyExtractor[A, K])(implicit ops: CacheOps, tA: TypeTagTree[A], tB: TypeTagTree[Downstream[A, K, V]], tC: TypeTagTree[Upstream[A, K, V]], tD: TypeTagTree[(A, V)]): Step[A, K, V] =
    new CacheStep[A, K, V](keyExtractor, ops, tA, tB, tC, tD)

  private class CacheStep[A, K, V](keyExtractor: KeyExtractor[A, K], ops: CacheOps, tA: TypeTagTree[A], tB: TypeTagTree[Downstream[A, K, V]], tC: TypeTagTree[Upstream[A, K, V]], tD: TypeTagTree[(A, V)]) extends Step[A, K, V] {
    val metadata = immutable.Seq(Metadata(Macros.nameOf[Cache.type], tA, tB, tC, tD))
    val downstream: LinkDownstream[A, Downstream[A, K, V]] = lookup
    val upstream: LinkUpstream[Upstream[A, K, V], (A, V)] = store
    val connections = immutable.Seq(this)
    val lifted = false

    private def lookup(given: A): Downstream[A, K, V] = {
      val key = keyExtractor(given)
      val result: Option[V] = ops.get[K, V](key)
      val mapped =
        if (result.isDefined)
          ShortCircuit((given, key, result))
        else
          MapAndProcess((given, key, None))
      mapped
    }

    private def store(entry: Upstream[A, K, V]): (A, V) = entry match {
      case ShortCircuit((given, _, value)) =>
        (given, value)

      case MapAndProcess((given, key, value)) =>
        (given, ops.put(key, value))
    }
  }
}

object KeyValueProcessor {
  import scala.collection._

  type Downstream[A, K, V] = ProxyPass[(A, K, Option[V])]
  type Upstream[A, K, V] = ProxyPass[(A, K, V)]

  val NAME = Macros.nameOf[KeyValueProcessor.type]

  def apply[A, K, V](processor: K => V)(implicit tA: TypeTagTree[Downstream[A, K, V]], tB: TypeTagTree[Upstream[A, K, V]]): ProxyStep[Downstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V]] =
    apply(NAME)(processor)(tA, tB)

  def apply[A, K, V](name: String)(processor: K => V)(implicit tA: TypeTagTree[Downstream[A, K, V]], tB: TypeTagTree[Upstream[A, K, V]]): ProxyStep[Downstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V]] =
    ProxyStep(Metadata(name, tA, tB, tB, tB))(process(processor), identity)

  private def process[A, K, V](processor: K => V)(pass: Downstream[A, K, V]): Upstream[A, K, V] =
    pass mapPF {
      case (given, key, Some(value)) =>
        (given, key, value)

      case (given, key, _) =>
        (given, key, processor(key))
    }

}

object Project {
  import KeyValueProcessor._

  val NAME = Macros.nameOf[Project.type]

  def upstreamValue[X, A, V](implicit tA: TypeTagTree[X], tC: TypeTagTree[(A, V)], tD: TypeTagTree[V]): ProxyStep[X, X, (A, V), V] =
    upstreamValue(NAME)(tA, tC, tD)

  def upstreamValue[X, A, V](name: String)(implicit tA: TypeTagTree[X], tC: TypeTagTree[(A, V)], tD: TypeTagTree[V]): ProxyStep[X, X, (A, V), V] =
    ProxyStep(Metadata(name, tA, tA, tC, tD))(identity, projectValue)

  def projectValue[A, V](in: (A, V)): V = {
    val (_, value) = in
    value
  }
}