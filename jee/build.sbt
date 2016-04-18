scalaVersion := "2.11.8"
name := "playground"
version := "0.20160418"

enablePlugins(JettyPlugin)

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8"
libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.2"
libraryDependencies += "org.flywaydb" % "flyway-core" % "4.0"
libraryDependencies += "com.h2database" % "h2" % "1.4.191"
libraryDependencies += "org.scalikejdbc" % "scalikejdbc_2.11" % "2.3.5"
libraryDependencies += "commons-io" % "commons-io" % "2.4"
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.38"
libraryDependencies += "joda-time" % "joda-time" % "2.9.3"
libraryDependencies += "org.scalatra" % "scalatra_2.11" % "2.4.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2"
libraryDependencies += "org.slf4j" % "jcl-over-slf4j" % "1.7.21"

libraryDependencies += "org.jsoup" % "jsoup" % "1.9.1"
libraryDependencies += "com.mashape.unirest" % "unirest-java" % "1.4.9"

libraryDependencies ++= Seq(
  "poi", "poi-ooxml"
).map("org.apache.poi" % _ % "3.14")

libraryDependencies ++= Seq(
  "jetty-webapp","jetty-plus"
).map("org.eclipse.jetty" % _ % "9.2.15.v20160210")

assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".properties" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".xml" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".types" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  case "application.conf"                            => MergeStrategy.concat
  case "unwanted.txt"                                => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion) //,
    //buildInfoPackage := "buildinfo"
  )
