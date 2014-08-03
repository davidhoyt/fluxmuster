package com.github.davidhoyt

package object fluxmuster2 {
  type LinkDownstream[-A, +B] = A => B
  type LinkUpstream[-C, +D] = C => D

}
