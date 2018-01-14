name := """MultiTest"""

version := "1.0"

scalaVersion := "2.11.7"

// scalacOptions ++= Seq("-unchecked","-deprecation","-feature")

// libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.12"

// libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"

//  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
//  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",

// libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

// resolvers += Resolver.sonatypeRepo("public")

// assemblyMergeStrategy in assembly := {
//   case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
//   case PathList(ps @ _*) if ps.last endsWith ".properties" => MergeStrategy.first
//   case PathList(ps @ _*) if ps.last endsWith ".xml" => MergeStrategy.first
//   case PathList(ps @ _*) if ps.last endsWith ".types" => MergeStrategy.first
//   case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
//   case "application.conf"                            => MergeStrategy.concat
//   case "unwanted.txt"                                => MergeStrategy.discard
//   case x =>
//     val oldStrategy = (assemblyMergeStrategy in assembly).value
//     oldStrategy(x)
// }

assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".properties" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".xml" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".types" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  case "application.conf"                            => MergeStrategy.concat
  case "unwanted.txt"                                => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
