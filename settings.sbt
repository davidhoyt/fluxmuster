import SimpleSettings._

primarySettings in Global := primary(
    name             = "Encabulator"
  , companyName      = "HoytSoft"
  , organization     = "com.github.davidhoyt"
  , homepage         = ""
  , vcsSpecification = ""
)

compilerSettings in Global := compiling(
    scalaVersion  = "2.10.4"
  , scalacOptions = Seq("-deprecation", "-unchecked", "-feature")
)

scaladocSettings in Global := scaladocs(
    options            = Seq("-groups", "-implicits")
  , useAutoApiMappings = true
  , baseApiUri         = ""
  , apiMappings        = Map(
      "com.typesafe"        % "config"      -> "http://typesafehub.github.io/config/latest/api/"
    , "com.typesafe.akka"   % "akka"        -> {spec: ArtifactSpec => s"http://doc.akka.io/api/akka/${spec.revision}/"}
    , "org.scalacheck"     %% "scalacheck"  -> "http://scalacheck.org/files/scalacheck_2.11-1.11.4-api/"
    , "com.netflix.rxjava" %% "rxjava-scala"-> "http://rxscala.github.io/scaladoc/"
    , "com.newzly"         %% "phantom-dsl" -> "http://newzly.github.io/phantom/api/"
  )
)

mavenSettings in Global := maven(
  license(
      name  = "The Apache Software License, Version 2.0"
    , url   = "http://www.apache.org/licenses/LICENSE-2.0.txt"
  ),
  developer(
      id              = "David Hoyt"
    , name            = "David Hoyt"
    , email           = "dhoyt@hoytsoft.org"
    , url             = "http://www.hoytsoft.org/"
    , organization    = "HoytSoft"
    , organizationUri = "http://www.hoytsoft.org/"
    , roles           = Seq("architect", "developer")
  )
)

publishSettings in Global := publishing(
    releaseCredentialsID  = "sonatype-nexus-staging"
  , releaseRepository     = "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  , snapshotCredentialsID = "sonatype-nexus-snapshots"
  , snapshotRepository    = "https://oss.sonatype.org/content/repositories/snapshots"
  , signArtifacts         = true
)

