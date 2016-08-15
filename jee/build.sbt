enablePlugins(BuildInfoPlugin)
enablePlugins(JettyPlugin)

// http://stackoverflow.com/questions/25665848/how-to-load-setting-values-from-a-java-properties-file
val buildProperties = settingKey[java.util.Properties]("The build properties")

buildProperties := {
  val prop = new java.util.Properties()
  IO.load(prop, new File("project%sbuild.properties".format(System.getProperty("file.separator"))))
  prop
}

name := Option(buildProperties.value.getProperty("name")).getOrElse("playground")
scalaVersion := Option(buildProperties.value.getProperty("scalaVersion")).getOrElse("2.11.8")
version := Option(buildProperties.value.getProperty("version")).getOrElse("0.20160809")
scalacOptions ++= Seq("-feature", "-deprecation")
mainClass in (Compile, run) := Some("Main")

parallelExecution in Test := false

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8" // http://mvnrepository.com/artifact/org.scala-lang/scala-compiler
libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.2" // http://mvnrepository.com/artifact/org.apache.httpcomponents/httpclient
libraryDependencies += "org.flywaydb" % "flyway-core" % "4.0.3" // http://mvnrepository.com/artifact/org.flywaydb/flyway-core
libraryDependencies += "com.h2database" % "h2" % "1.4.192" // http://mvnrepository.com/artifact/com.h2database/h2
libraryDependencies += "commons-io" % "commons-io" % "2.5" // http://mvnrepository.com/artifact/commons-io/commons-io
libraryDependencies += "mysql" % "mysql-connector-java" % "6.0.3" // http://mvnrepository.com/artifact/mysql/mysql-connector-java
libraryDependencies += "joda-time" % "joda-time" % "2.9.4"  // http://mvnrepository.com/artifact/joda-time/joda-time
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7" // http://mvnrepository.com/artifact/ch.qos.logback/logback-classic
libraryDependencies += "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2" // http://mvnrepository.com/artifact/com.typesafe.scala-logging/scala-logging-slf4j_2.11
libraryDependencies += "org.slf4j" % "jcl-over-slf4j" % "1.7.21"  // http://mvnrepository.com/artifact/org.slf4j/jcl-over-slf4j
libraryDependencies += "org.jsoup" % "jsoup" % "1.9.2"  // http://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "com.mashape.unirest" % "unirest-java" % "1.4.9" // http://mvnrepository.com/artifact/com.mashape.unirest/unirest-java
libraryDependencies += "com.opencsv" % "opencsv" % "3.8" // http://mvnrepository.com/artifact/com.opencsv/opencsv
libraryDependencies += "org.apache.velocity" % "velocity" % "1.7"  // http://mvnrepository.com/artifact/org.apache.velocity/velocity
libraryDependencies += "org.pegdown" % "pegdown" % "1.6.0" // http://mvnrepository.com/artifact/org.pegdown/pegdown
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.4.1.201607150455-r" // http://mvnrepository.com/artifact/org.eclipse.jgit/org.eclipse.jgit
libraryDependencies += "org.jsoup" % "jsoup" % "1.9.2" // http://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.8" // http://mvnrepository.com/artifact/com.yahoo.platform.yui/yuicompressor
libraryDependencies += "com.jakewharton.fliptables" % "fliptables" % "1.0.2" // https://mvnrepository.com/artifact/com.jakewharton.fliptables/fliptables

libraryDependencies ++= Seq(
  "scalikejdbc_2.11","scalikejdbc-syntax-support-macro_2.11"
).map("org.scalikejdbc" % _ % "2.4.2")

libraryDependencies ++= Seq(
  "scalatra_2.11", "scalatra-json_2.11"
).map("org.scalatra" % _ % "2.4.1") // http://mvnrepository.com/artifact/org.scalatra/scalatra_2.11

libraryDependencies ++= Seq(
  "json4s-jackson_2.11", "json4s-ext_2.11"
).map("org.json4s" % _ % "3.4.0") // http://mvnrepository.com/artifact/org.json4s/json4s-jackson_2.11

libraryDependencies ++= Seq(
  "poi", "poi-ooxml"
).map("org.apache.poi" % _ % "3.14")  // http://mvnrepository.com/artifact/org.apache.poi/poi

libraryDependencies ++= Seq(
  "jetty-webapp","jetty-plus"
).map("org.eclipse.jetty" % _ % "9.3.11.v20160721") // http://mvnrepository.com/artifact/org.eclipse.jetty/jetty-webapp

libraryDependencies ++= Seq(
  "trireme-core", // https://mvnrepository.com/artifact/io.apigee.trireme/trireme-core
  "trireme-node10src" // https://mvnrepository.com/artifact/io.apigee.trireme/trireme-node10src
).map("io.apigee.trireme" % _ % "0.8.9")

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test // http://mvnrepository.com/artifact/com.novocode/junit-interface
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % Test // http://mvnrepository.com/artifact/org.scalatest/scalatest_2.11

libraryDependencies ++= Seq(
  "selenium-support", // http://mvnrepository.com/artifact/org.seleniumhq.selenium/selenium-support
  "selenium-chrome-driver"  // http://mvnrepository.com/artifact/org.seleniumhq.selenium/selenium-chrome-driver
).map("org.seleniumhq.selenium" % _ % "2.53.1" % Test)

val e2etest = taskKey[Unit]("Execute e2e.[projectname].* tests")
e2etest := (Def.taskDyn {
  val nameStr = name.value
  Def.task {
    (testOnly in Test).toTask(s" e2e.${nameStr}.*").value
  }
}).value

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
mainClass in assembly := Some("ConsoleMain")
test in assembly := {}

initialCommands in console := (((new File(".") / "src/main/scala/modules") * "*" * "package.scala"):PathFinder).get.map("import modules." + _.getParentFile.getName + "._").mkString(";") + ";initDefaultDatabase;migrateDefaultDatabase;import scalikejdbc._"

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion,initialCommands in console) //buildInfoPackage := "buildinfo"
