package modules.cms

class ScalatraBootstrap extends org.scalatra.LifeCycle {


  override def init(context: javax.servlet.ServletContext):Unit = {
    val config = modules.config.get

    val (url, user, password) = (config.cmsDatabaseURL, config.cmsDatabaseUser, config.cmsDatabasePassword)
    val flyway = new org.flywaydb.core.Flyway()
    flyway.setLocations("db/cms/migration")
    flyway.setValidateOnMigrate(false)
    flyway.setDataSource(url, user, password)
    flyway.migrate()

    scalikejdbc.ConnectionPool.add("cms", url, user, password)

    context.mount(classOf[modules.cms.Api], "/cms/api.php/*")
  }
}
