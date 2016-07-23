scalaVersion := "2.11.8"
name := "playground"
version := "0.20160723"

enablePlugins(JettyPlugin)

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8" // http://mvnrepository.com/artifact/org.scala-lang/scala-compiler
libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.2" // http://mvnrepository.com/artifact/org.apache.httpcomponents/httpclient
libraryDependencies += "org.flywaydb" % "flyway-core" % "4.0.3" // http://mvnrepository.com/artifact/org.flywaydb/flyway-core
libraryDependencies += "com.h2database" % "h2" % "1.4.192" // http://mvnrepository.com/artifact/com.h2database/h2
libraryDependencies += "org.scalikejdbc" % "scalikejdbc_2.11" % "2.4.2" // http://mvnrepository.com/artifact/org.scalikejdbc/scalikejdbc_2.11
libraryDependencies += "commons-io" % "commons-io" % "2.5" // http://mvnrepository.com/artifact/commons-io/commons-io
libraryDependencies += "mysql" % "mysql-connector-java" % "6.0.3" // http://mvnrepository.com/artifact/mysql/mysql-connector-java
libraryDependencies += "joda-time" % "joda-time" % "2.9.4"  // http://mvnrepository.com/artifact/joda-time/joda-time
libraryDependencies += "org.scalatra" % "scalatra_2.11" % "2.4.1" // http://mvnrepository.com/artifact/org.scalatra/scalatra_2.11

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7" // http://mvnrepository.com/artifact/ch.qos.logback/logback-classic
libraryDependencies += "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2" // http://mvnrepository.com/artifact/com.typesafe.scala-logging/scala-logging-slf4j_2.11
libraryDependencies += "org.slf4j" % "jcl-over-slf4j" % "1.7.21"  // http://mvnrepository.com/artifact/org.slf4j/jcl-over-slf4j

libraryDependencies += "org.jsoup" % "jsoup" % "1.9.2"  // http://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "com.mashape.unirest" % "unirest-java" % "1.4.9" // http://mvnrepository.com/artifact/com.mashape.unirest/unirest-java
libraryDependencies += "com.opencsv" % "opencsv" % "3.8" // http://mvnrepository.com/artifact/com.opencsv/opencsv


libraryDependencies ++= Seq(
  "poi", "poi-ooxml"
).map("org.apache.poi" % _ % "3.14")  // http://mvnrepository.com/artifact/org.apache.poi/poi

libraryDependencies ++= Seq(
  "jetty-webapp","jetty-plus"
).map("org.eclipse.jetty" % _ % "9.2.17.v20160517") // http://mvnrepository.com/artifact/org.eclipse.jetty/jetty-webapp

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
