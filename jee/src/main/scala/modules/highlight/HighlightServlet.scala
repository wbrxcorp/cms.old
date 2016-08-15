package modules.highlight

import java.io.File
import org.apache.commons.io.IOUtils
import modules.common.using

class Entry(file:File) {
  def getName = if (isDir) file.getName + '/' else file.getName
  def isDir = file.isDirectory
}

class HighlightServlet extends modules.scalatra.JsonSupport with modules.scalatra.LogErrors {
  import modules.velocity._
  import modules.file._
  import modules.pegdown._

  val highlightRoot = modules.config.get.highlightRoot
  val defaultCharset = "UTF-8"

  def processDirectory(path:String, dir:File):Unit = {
    val template = loadResource("/WEB-INF/templates/highlight_dir.vm")
    contentType = "text/html; charset=UTF-8"
    val entries = dir.listFiles.map(new Entry(_)).filter { entry =>
      Seq(".git/", "target/","bin/",".classpath").forall(entry.getName != _) &&
      Seq(".lock",".jar",".phar",".war",".md").forall(!entry.getName.toLowerCase.endsWith(_)) &&
      (entry.isDir || determineLanguageBySuffix(entry.getName).nonEmpty)
    }
    response.getWriter.write(evaluateVelocityTemplate(template, Map("entries"->entries, "path"->path), "processDirectory"))
  }

  def processFile(path:String, file:File):Unit = {
    val template = loadResource("/WEB-INF/templates/highlight_file.vm")
    contentType = "text/html; charset=UTF-8"
    val source = using(new java.io.FileInputStream(file)) { is =>
      org.apache.commons.lang.StringEscapeUtils.escapeHtml(IOUtils.toString(is, defaultCharset))
    }
    val language = determineLanguageBySuffix(file.getName).getOrElse(halt(404, "File not found")/*非対応ファイル*/)

    val mdPath = file.getAbsolutePath.split("\\.(?=[^\\.]+$)")(0) + ".md" // ファイル名のサフィックスを差し替え
    val mdFile = new File(mdPath)
    val defaultDescription = "%s のサンプルプログラム %s".format(language._1, file.getName)
    val notation = if (mdFile.isFile) {
      val (md, meta) = extractMetadataFromMarkdown(using(new java.io.FileInputStream(mdFile)) { is => IOUtils.toString(is, defaultCharset) })
      (renderMarkdown(md, MyLinkRenderer), meta.get("title").getOrElse(file.getName), meta.get("description").getOrElse(defaultDescription))
    } else {
      ("", file.getName, defaultDescription)
    }

    val variables = Map(
      "title"->notation._2,
      "description"->notation._3,
      "content"->notation._1,
      "source"->source,
      "lastUpdate"->new java.util.Date(file.lastModified),
      "path"->path,
      "contextPath"->servletContext.getContextPath,
      "servletPath"->request.getServletPath,
      "language"->language._1,
      "highlight"->language._2
    )
    response.getWriter.write(evaluateVelocityTemplate(template, variables, "processFile"))
  }

  def loadResource(path:String, charset:String=defaultCharset):String = {
    // まずはwar外のファイルを探す
    val file = new File(joinPath(Seq(highlightRoot, "jee/src/main/webapp", path)))
    if (file.isFile) {
      using(new java.io.FileInputStream(file)) { in => IOUtils.toString(in, charset) }
    } else {
      // なかったらwar内のファイルを使う
      using(Option(servletContext.getResourceAsStream(path)).getOrElse(halt(404, "Page template %s not found".format(path)))) { in =>
        IOUtils.toString(in, charset)
      }
    }
  }

  get("*") {
    logger.debug("servletPath:%s, pathInfo:%s".format(request.getServletPath, request.getPathInfo))
    val path = request.getPathInfo
    val realPath = joinPath(Seq(highlightRoot, path))
    logger.debug(realPath)
    val file = new java.io.File(realPath)
    if (file.isDirectory) {
      processDirectory(path, file)
    } else if (file.isFile) {
      processFile(path, file)
    } else {
      halt(404, "Not found")
    }
  }

  get("/search") {
    redirect("https://www.google.co.jp/search?q=site%3Awww.walbrix.com%2Fplayground%2F+" + java.net.URLEncoder.encode(params("q"), "UTF-8"))
  }

  get("/wikipedia/:pagename") {
    val ipAddress = request.getRemoteAddr
    modules.wikipedia.getWikipediaEntry(params("pagename"), ipAddress)
  }

}
