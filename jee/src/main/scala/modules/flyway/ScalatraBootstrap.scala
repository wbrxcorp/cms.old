package modules.flyway

class ScalatraBootstrap extends org.scalatra.LifeCycle {

  override def init(context: javax.servlet.ServletContext):Unit = {
    migrateDefaultDatabase
  }
}
