package modules.config

trait Config {
  def projectName:String = buildinfo.BuildInfo.name

  // Jetty設定
  def webAppPort:Int = 51234 + Integer.parseInt(modules.hash.sha1(projectName).substring(0,3), 16)// プロジェクト名のsha1sum文字列から3文字(4096種類)取ってポート番号にする
  def webAppDirs:Array[String] = Array("src/main/webapp", "../htdocs")

  // Scalatra設定
  def additionalScalatraBootstrapClasses:Array[Class[_ <: org.scalatra.LifeCycle]] = Array(
    classOf[modules.h2.ScalatraBootstrap],
    classOf[modules.scalikejdbc.ScalatraBootstrap],
    classOf[modules.flyway.ScalatraBootstrap],
    classOf[modules.highlight.ScalatraBootstrap],
    classOf[modules.playground.ScalatraBootstrap]
  )

  // ScalikeJDBC設定
  def scalikeJDBCLogLevel = 'DEBUG

  // デフォルトデータベース設定
  def defaultDatabaseURL:String = modules.h2.getMySQLCompatibleH2InMemoryDatabaseURL(projectName)
  def defaultDatabaseMigrationLocations:Option[String] = Some("db/%s/migration".format(projectName))
  def defaultDatabaseUser:String = "sa"
  def defaultDatabasePassword:String = ""

  // ソースコードハイライトシステム設定
  def highlightRoot:String = ".."
}

// warでTomcatとかにデプロイされた時用のデフォルト設定
trait DefaultDefaultConfig extends Config {
  // ソースコードハイライトシステム設定
  override def highlightRoot:String = "/home/wbrxcorp/playground"
}

// sbt runされた時用のデフォルト設定
trait DefaultStandaloneConfig extends Config {
}

object DefaultDefaultConfig extends DefaultDefaultConfig {
}

object DefaultStandaloneConfig extends DefaultStandaloneConfig {
}
