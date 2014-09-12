package com.github.davidhoyt.fluxmusterOrig

import net.sf.ehcache.{Ehcache => ActualEhcache, Element, CacheManager}

object Ehcache {
  def availableCaches: Seq[String] =
    CacheManager.getInstance().getCacheNames.toList

  def apply(cacheId: String): CacheOps =
    apply(
      Option(CacheManager.getInstance().getEhcache(cacheId))
        .getOrElse(throw new IllegalStateException(s"Cache configuration not found for cacheId $cacheId"))
    )

  def apply(cache: => ActualEhcache): CacheOps =
    new CacheOps {
      def get[A, B](key: A): Option[B] =
        Option(cache.get(key)).map(_.getObjectValue.asInstanceOf[B])

      def put[A, B](key: A, value: B): B = {
        cache.put(new Element(key, value))
        value
      }
    }
}
