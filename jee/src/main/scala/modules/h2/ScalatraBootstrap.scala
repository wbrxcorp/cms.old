package modules.h2

class ScalatraBootstrap extends org.scalatra.LifeCycle {

  override def init(context: javax.servlet.ServletContext) {
    context.mount(classOf[org.h2.server.web.WebServlet], "/h2/*")
  }
}
