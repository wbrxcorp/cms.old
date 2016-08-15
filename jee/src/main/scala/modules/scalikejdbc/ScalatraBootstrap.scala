package modules.scalikejdbc

class ScalatraBootstrap extends org.scalatra.LifeCycle {

  override def init(context: javax.servlet.ServletContext) {
    initDefaultDatabase
  }
}
