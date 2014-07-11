import SimpleSettings._

lazy val _all = module("all")(
  base      = ".",
  publish   = false,
  aggregate = Seq(macros, core)
)

lazy val core = module("fluxmuster-core")("core")
                  .dependsOn(macros % "compile")

lazy val macros = module("fluxmuster-macros")("macros")

