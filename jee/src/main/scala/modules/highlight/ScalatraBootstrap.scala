package modules.highlight

class ScalatraBootstrap extends org.scalatra.LifeCycle {
  override def init(context: javax.servlet.ServletContext) {
    context.mount(classOf[modules.highlight.HighlightServlet], "/highlight/*")
  }
}
