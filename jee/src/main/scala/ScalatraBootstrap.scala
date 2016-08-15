class ScalatraBootstrap extends org.scalatra.LifeCycle {

  override def init(context: javax.servlet.ServletContext):Unit = {
    // execute additional scalatrabootstraps
    modules.config.get.additionalScalatraBootstrapClasses.foreach(_.newInstance.init(context))
  }
}
