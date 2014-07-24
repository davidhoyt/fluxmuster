package com.github.davidhoyt

import scala.collection.generic.CanBuildFrom

package object fluxmuster {
  implicit class SeqTypeTagTreeEnhancements(val xs: Seq[TypeTagTree[_]]) extends AnyVal {
    import scala.reflect.runtime.universe._

    import scala.language.higherKinds

    def types: Seq[Type] =
      xs map (_.tpe)
  }
}
