package com.github.davidhoyt.fluxmuster

import com.codahale.metrics.MetricRegistry

//this won't do -- need separate Timer(), Counter(), etc.
//where each is a ProxyStep

trait MetricsOps {
  def counter(name: String): CounterMetric

  trait CounterMetric {
    type Self <: CounterMetric

    val name: String

    def ++ : Self = increment(1L)
    def +=(by: Long): Self = increment(by)
    def increment(by: Long = 1): Self
    def -- : Self = decrement(1L)
    def -=(by: Long): Self = decrement(by)
    def decrement(by: Long = 1): Self
  }
}

object Metrics {
  def apply(implicit ops: MetricsOps) = {

  }
}

object CodaHale {
  def apply(implicit registry: MetricRegistry = new MetricRegistry()): MetricsOps =
    new CodaHale(registry)
}

sealed class CodaHale(val registry: MetricRegistry) extends MetricsOps {
  def counter(name: String) =
    new Counter(name)

  class Counter(val name: String) extends CounterMetric {
    type Self = Counter

    val counter = registry.counter(name)

    def increment(by: Long) = {
      counter.inc(by)
      this
    }

    def decrement(by: Long) = {
      counter.dec(by)
      this
    }
  }
}