import SimpleSettings._

lazy val _all = module("all")(
  base      = ".",
  publish   = false,
  aggregate = Seq(core)
)

lazy val core = module("encabulator-core")("core")

