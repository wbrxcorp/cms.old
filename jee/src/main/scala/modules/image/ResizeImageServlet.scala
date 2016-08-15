package modules.image

import javax.servlet.http.HttpServletResponse

class ResizeImageServlet extends javax.servlet.http.HttpServlet with com.typesafe.scalalogging.slf4j.LazyLogging {
  private val pattern = """^/(\d+)[xX](\d+)$""".r

  override def doPost(request:javax.servlet.http.HttpServletRequest, response:HttpServletResponse):Unit = {
    val (maxWidth, maxHeight) = Option(request.getPathInfo).getOrElse("") match {
      case "/" => (None, None)
      case pattern(width, height) => (Some(width.toInt), Some(height.toInt))
      case _ =>
        response.sendError(HttpServletResponse.SC_NOT_FOUND)
        return
    }
    val (image, format) = try {
      resizeImage(request.getInputStream, maxWidth, maxHeight)
    }
    catch {
      case e:IllegalArgumentException =>
        response.sendError(HttpServletResponse.SC_BAD_REQUEST)
        return
    }

    response.setContentType("text/plain")
    val writer = response.getWriter
    writer.write(toDataURI(format, image))
    writer.flush
  }
}
