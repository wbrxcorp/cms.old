class ScalatraBootstrap extends org.scalatra.LifeCycle with com.typesafe.scalalogging.slf4j.LazyLogging {

  override def init(context: javax.servlet.ServletContext):Unit = {
    // execute additional scalatrabootstraps
    modules.config.get.additionalScalatraBootstrapClasses.foreach { sbs=>
      logger.info("Loading Sub-ScalatraBootstrap '%s'".format(sbs.getName))
      sbs.newInstance.init(context)
    }
  }
}
