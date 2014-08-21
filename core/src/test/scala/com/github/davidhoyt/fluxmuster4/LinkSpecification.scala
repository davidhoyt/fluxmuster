package com.github.davidhoyt.fluxmuster4

import com.github.davidhoyt.fluxmuster.Macros
import org.scalacheck.Properties

object LinkSpecification extends Properties(Macros.simpleNameOf[Link.type]) {
  import org.scalacheck.Prop
  import generators.LinkGenerators._

  property("Apply, run, and runAny all return the same values") = Prop.forAll { (genLink: GeneratedLink) =>
    val GeneratedLink(default, link) = genLink

    val validChainSize   = link.chain.size > 0
    val resultFromApply  = link(default)
    val resultFromRun    = link.run(default)
    val resultFromRunAny = link.runAny(default)

    validChainSize && (resultFromApply == resultFromRun) && (resultFromApply == resultFromRunAny)
  }

}
