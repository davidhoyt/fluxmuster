package com.github.davidhoyt.fluxmuster

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
  type Specification[A, K, V] = ProxySpecification[A, Downstream[A, K, V], Upstream[A, K, V], (A, V)]

  //Provide a way to convey cache eviction, replace, etc.?

  def apply[K, V](implicit ops: CacheOps): Specification[K, K, V] =
    new CacheSpecification[K, K, V](identity, ops)

  @scala.annotation.implicitNotFound("Please specify a cache typeclass to use.")
  def apply[A, K, V](keyExtractor: KeyExtractor[A, K])(implicit ops: CacheOps): Specification[A, K, V] =
    new CacheSpecification[A, K, V](keyExtractor, ops)

  private class CacheSpecification[A, K, V](keyExtractor: KeyExtractor[A, K], ops: CacheOps) extends Specification[A, K, V] {
    val metadata = immutable.Seq(Macros.nameOf[Cache.type])
    val downstream: LinkDownstream[A, Downstream[A, K, V]] = lookup
    val upstream: LinkUpstream[Upstream[A, K, V], (A, V)] = store
    val connections = immutable.Seq(this)

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

  def apply[A, K, V](processor: K => V): ProxySpecification[Downstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V]] =
    apply(NAME)(processor)

  def apply[A : TypeData, K : TypeData, V : TypeData](name: String)(processor: K => V): ProxySpecification[Downstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V], Upstream[A, K, V]] =
    ProxySpecification(Metadata(name, TypeData[Downstream], TypeData[Upstream], TypeData[Upstream], TypeData[Upstream], additionalTypes = immutable.Seq(implicitly[TypeData[A]], implicitly[TypeData[K]], implicitly[TypeData[V]])))(process(processor), identity)

  private def process[A, K, V](processor: K => V)(pass: Downstream[A, K, V]): Upstream[A, K, V] =
    pass mapPF {
      case (given, key, Some(value)) =>
        (given, key, value)

      case (given, key, _) =>
        (given, key, processor(key))
    }

}

object Projection {
  import KeyValueProcessor._
  def upstreamTuple2[X, A, V]: ProxySpecification[X, X, (A, V), V] =
    ProxySpecification(Macros.nameOf[Projection.type])(identity, projectTuple2)

  def projectTuple2[A, V](in: (A, V)): V = {
    val (_, value) = in
    value
  }
}