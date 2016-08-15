package modules

import org.eclipse.jetty.server.Server

package object jetty {
  def startWebApp(port:Int, resources:Array[String]):Server = {
    val root = new org.eclipse.jetty.webapp.WebAppContext
    root.setBaseResource(new org.eclipse.jetty.util.resource.ResourceCollection(resources))
    root.setContextPath("/")
    root.setClassLoader(new java.net.URLClassLoader(new Array[java.net.URL](0), this.getClass().getClassLoader()))
    root.setInitParameter("org.eclipse.jetty.servlet.Default.useFileMappedBuffer", "false")

    val server = new Server(port)
    server.setHandler(root)
    server.start
    server
  }

  def startWebApp(port:Int, resource:String):Server = startWebApp(port, Array(resource))

  def startServer:(Server, Int) = {
    val config = modules.config.get
    val port = config.webAppPort
    (startWebApp(port, config.webAppDirs), port)
  }

}
