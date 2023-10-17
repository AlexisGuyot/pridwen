scalaVersion := "2.13.8"

name := "pridwen"
organization := "fr.u-bourgogne.lib.sd"
version := "1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

// Shapeless
resolvers ++= Resolver.sonatypeOssRepos("releases")
resolvers ++= Resolver.sonatypeOssRepos("snapshots")
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

// Scala Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14"


// Scala Reflect
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

// Refined
libraryDependencies ++= Seq(
  "eu.timepit" %% "refined"                 % "0.11.0",
  "eu.timepit" %% "refined-cats"            % "0.11.0", // optional
  "eu.timepit" %% "refined-eval"            % "0.11.0", // optional, JVM-only
  "eu.timepit" %% "refined-jsonpath"        % "0.11.0", // optional, JVM-only
  "eu.timepit" %% "refined-pureconfig"      % "0.11.0", // optional, JVM-only
  "eu.timepit" %% "refined-scalacheck"      % "0.11.0", // optional
  "eu.timepit" %% "refined-scalaz"          % "0.11.0", // optional
  "eu.timepit" %% "refined-scodec"          % "0.11.0", // optional
  "eu.timepit" %% "refined-scopt"           % "0.11.0", // optional
  "eu.timepit" %% "refined-shapeless"       % "0.11.0"  // optional
)

// Breeze
libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "1.1",
  "org.scalanlp" %% "breeze-natives" % "1.1",
  "org.scalanlp" %% "breeze-viz" % "1.1"
)

// Gephi
resolvers ++= Seq(
  "gephi-thirdparty" at "https://raw.github.com/gephi/gephi/mvn-thirdparty-repo/"
)

libraryDependencies += "org.gephi" % "gephi-toolkit" % "0.10.1" classifier "all"

//scalaVersion := "2.13.3"
// Spark
libraryDependencies += "org.apache.spark" %% "spark-core" % "3.4.0"
libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.4.0"

// UPickles + OS-lib
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.3"
