package profiles.default

// default プロファイルは warでアプリケーションサーバにデプロイされた時を想定

// create a copy of this as "Config" object

object ConfigSample extends modules.config.DefaultDefaultConfig {
  override def additionalScalatraBootstrapClasses:Array[Class[_ <: org.scalatra.LifeCycle]] = Array(
    classOf[modules.scalikejdbc.ScalatraBootstrap],
    classOf[modules.flyway.ScalatraBootstrap]
  )
  override def defaultDatabaseURL:String = modules.mysql.getMySQLDatabaseURL(projectName)
  override def defaultDatabaseUser:String = projectName
  override def defaultDatabasePassword:String = "secret"
}
