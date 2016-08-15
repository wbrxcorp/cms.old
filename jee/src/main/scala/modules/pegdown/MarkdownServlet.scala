package modules.markdown

import javax.servlet.ServletContext
import javax.servlet.http.HttpServletResponse

import org.apache.commons.io.IOUtils

import modules.common.using

class MarkdownServlet extends javax.servlet.http.HttpServlet with com.typesafe.scalalogging.slf4j.LazyLogging {
  def loadResource(servletContext:ServletContext,path:String):Option[String] = {
    Option(servletContext.getResource(path)).map { resource =>
      using (resource.openStream) { stream => IOUtils.toString(stream, "utf-8") }
    }
  }

  override def doGet(request:javax.servlet.http.HttpServletRequest, response:HttpServletResponse):Unit = {
    val servletContext = getServletContext
    Option(servletContext.getResource(request.getServletPath)) match {
      case Some(resource) =>
        using(resource.openStream) { stream =>
          response.setContentType("text/html")
          response.setCharacterEncoding("utf-8")
          val template = loadResource(servletContext, "/WEB-INF/templates/markdown.vm").orElse(loadResource(servletContext, "/WEB-INF/templates/markdown-default.vm")).getOrElse("<html><body>$content</body></html>")
          val (markdown, metadata) = modules.pegdown.extractMetadataFromMarkdown(IOUtils.toString(stream, "utf-8"))
          val variables = metadata + ("content"->modules.pegdown.renderMarkdown(markdown))
          val html = modules.velocity.evaluateVelocityTemplate(template, variables, "markdown")
          IOUtils.write(html, response.getOutputStream, "utf-8")
        }
      case None =>
        response.sendError(HttpServletResponse.SC_NOT_FOUND)
    }
  }
}
