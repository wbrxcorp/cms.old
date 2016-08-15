package modules.blobstore

import javax.servlet.http.HttpServletResponse

class BlobServlet extends javax.servlet.http.HttpServlet with com.typesafe.scalalogging.slf4j.LazyLogging {
  val pattern = """^/(.+)\.(.+)$""".r

  override def doGet(request:javax.servlet.http.HttpServletRequest, response:HttpServletResponse):Unit = {
    val (contentType, content) = (Option(request.getPathInfo).getOrElse("") match {
      case pattern(id, suffix) =>
        //logger.info(id)
        modules.blobstore.getBlob(id)
      case _ => None
    }).getOrElse {
      response.sendError(HttpServletResponse.SC_NOT_FOUND)
      return
    }

    response.setContentType(contentType)
    response.setContentLength(content.length)
    val out = response.getOutputStream
    out.write(content)
    out.flush
  }
}
