
resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
  //"JBoss" at "http://repository.jboss.org/nexus/content/groups/public",
  "Kamon Repository" at "http://repo.kamon.io"
)

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "com.codahale.metrics" % "metrics-core" % "3.0.2",
  "com.codahale.metrics" % "metrics-json" % "3.0.2",
  "com.github.nscala-time" %% "nscala-time" % "1.2.0",
  "com.netflix.hystrix" % "hystrix-core" % "1.4.0-RC4",
  "com.netflix.hystrix" % "hystrix-metrics-event-stream" % "1.4.0-RC4",
  "com.netflix.rxjava" % "rxjava-scala" % "0.19.1",
  "com.palominolabs.metrics" % "metrics-new-relic" % "1.0.4",
  "com.typesafe" % "config" % "1.2.1",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "com.typesafe.akka" %% "akka-actor" % "2.3.3",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3.3",
  "io.kamon" %% "kamon-core"  % "0.3.1",
  "io.kamon" %% "kamon-spray" % "0.3.1",
  "io.kamon" %% "kamon-newrelic" % "0.3.1",
  "io.spray" % "spray-caching" % "1.3.1",
  "io.spray" % "spray-can" % "1.3.1",
  "io.spray" % "spray-routing" % "1.3.1",
  "nl.grons" %% "metrics-scala" % "3.2.0_a2.3",
  "org.json4s" %% "json4s-jackson" % "3.2.10",
  "org.json4s" %% "json4s-ext" % "3.2.10",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "net.ceedubs" %% "ficus" % "1.0.1",
  //
  "com.codahale.metrics" % "metrics-ehcache" % "3.0.2",
  "com.datastax.cassandra" % "cassandra-driver-core" % "2.0.2",
  "com.typesafe.play" %% "play-iteratees" % "2.2.2",
  "com.h2database" % "h2" % "1.4.178",
  "com.newzly" %% "phantom-dsl" % "0.8.0",
  "com.typesafe.slick" %% "slick" % "2.0.2",
  "com.zaxxer" % "HikariCP" % "1.4.0",
  "mysql" % "mysql-connector-java" % "5.1.31",
  "net.jpountz.lz4" % "lz4" % "1.2.0",
  "net.sf.ehcache" % "ehcache-core" % "2.6.9",
  "org.xerial.snappy" % "snappy-java" % "1.1.0.1"
)

