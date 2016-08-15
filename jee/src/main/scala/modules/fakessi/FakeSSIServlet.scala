package modules.fakessi

import javax.servlet.ServletContext
import javax.servlet.http.HttpServletResponse

import org.apache.commons.io.IOUtils

/**
  * Created by shimarin on 15/12/07.
  */
class FakeSSIServlet extends javax.servlet.http.HttpServlet {

  def loadResource(servletContext:ServletContext,path:String):String = {
    if (!path.startsWith("/")) return "include path name must start with '/'"
    Option(servletContext.getResource(path)).map { resource =>
      val stream = resource.openStream()
      try {
        IOUtils.toString(stream, "utf-8")
      }
      finally {
        stream.close
      }
    }.getOrElse("'%s' not found in SSI include statement")
  }

  override def doGet(request:javax.servlet.http.HttpServletRequest, response:HttpServletResponse):Unit = {
    val servletContext = getServletContext
    Option(servletContext.getResource(request.getServletPath)) match {
      case Some(resource) =>
        val stream = resource.openStream()
        response.setContentType("text/html")
        response.setCharacterEncoding("utf-8")
        val shtml = resolveShtml(IOUtils.toString(stream, "utf-8"), loadResource(servletContext, _))
        IOUtils.write(shtml, response.getOutputStream, "utf-8")
        stream.close
      case None =>
        response.sendError(HttpServletResponse.SC_NOT_FOUND)
    }
  }

}
