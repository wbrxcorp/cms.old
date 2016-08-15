package modules.jetty

object JavaCompat {
  def startServer:(org.eclipse.jetty.server.Server, java.lang.Integer) = {
    val (server, port) = modules.jetty.startServer
    (server, new java.lang.Integer(port))
  }
}
