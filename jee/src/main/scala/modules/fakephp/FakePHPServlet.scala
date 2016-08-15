package modules.fakephp

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import org.apache.commons.io.IOUtils

class FakePHPServlet extends javax.servlet.http.HttpServlet with com.typesafe.scalalogging.slf4j.LazyLogging {

  private def resolveSessionVariable(request:HttpServletRequest, sessionVariableName:String):Option[AnyRef] = {
    Option(request.getSession.getAttribute(sessionVariableName))
  }

  override def doGet(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    //println(request.getServletPath)
    Option(getServletContext.getResource(request.getServletPath)) match {
      case Some(resource) =>
        val stream = resource.openStream()
        response.setContentType("text/html")
        response.setCharacterEncoding("utf-8")
        val php = resolvePHP(IOUtils.toString(stream, "utf-8"), resolveSessionVariable(request, _))
        IOUtils.write(php, response.getOutputStream, "utf-8")
        stream.close
      case None =>
        response.sendError(HttpServletResponse.SC_NOT_FOUND)
    }
  }

}
