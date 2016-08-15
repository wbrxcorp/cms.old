package modules.cms

class ScalatraBootstrap extends org.scalatra.LifeCycle {


  override def init(context: javax.servlet.ServletContext):Unit = {
    context.mount(classOf[modules.cms.Api], "/api.php/*")
  }
}
