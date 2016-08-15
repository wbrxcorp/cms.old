package modules.yuicompressor

import java.io.FileNotFoundException
import javax.servlet.http.HttpServletResponse

import modules.common.using

class CompressFilter extends javax.servlet.Filter with com.typesafe.scalalogging.slf4j.LazyLogging {
  val charset:String = "UTF-8"
  private var context:javax.servlet.ServletContext = _
  private var compressorConfig:Config = Config()

  override def init(config: javax.servlet.FilterConfig): Unit = {
    this.context = config.getServletContext
    this.compressorConfig = Config(
      lineBreak = Option(config.getInitParameter("lineBreak")).map(_.toInt).getOrElse(compressorConfig.lineBreak),
      noMunge = Option(config.getInitParameter("noMunge")).map(_.toBoolean).getOrElse(compressorConfig.noMunge),
      preserveSemi = Option(config.getInitParameter("preserveSemi")).map(_.toBoolean).getOrElse(compressorConfig.preserveSemi),
      disableOptimizations = Option(config.getInitParameter("disableOptimizations")).map(_.toBoolean).getOrElse(compressorConfig.disableOptimizations)
    )
  }

  def getLstFile(lstFilePath:String):Option[(Seq[java.net.URL], Long)] = {
    Option(context.getResource(lstFilePath)).map { resource =>
      val conn = resource.openConnection
      val lastModified = conn.getLastModified
      using(conn.getInputStream) { in =>
        using(scala.io.Source.fromInputStream(in, charset)) { source =>
          (source.getLines.map(_.replaceFirst("""#.*$""","").trim).filter(_ != "").map { line =>
            val resourcePath = if (line.startsWith("/")) line else {
              modules.file.joinPath((lstFilePath.split('/').toSeq.dropRight(1) :+ "").mkString("/"), line)
            }
            Option(context.getResource(resourcePath)).getOrElse(throw new FileNotFoundException(resourcePath))
          }.toList, lastModified)
        }
      }
    }
  }

  override def doFilter(request: javax.servlet.ServletRequest, response: javax.servlet.ServletResponse, chain: javax.servlet.FilterChain): Unit = {
    val path = request.asInstanceOf[javax.servlet.http.HttpServletRequest].getServletPath
    val lstFilePath = path + ".lst"
    val pathLowerCase = path.toLowerCase
    logger.debug("processing %s".format(path))
    try {
      getLstFile(lstFilePath).map { case (resources, lstFileLastModified) =>
        response.setCharacterEncoding("UTF-8")
        response.setContentType(pathLowerCase.replaceFirst("""^.*\.""","") match {
          case "js" => "application/javascript"
          case "css" => "text/css"
          case _ => "text/plain"
        })
        val out = response.getWriter
        val loadedFiles = (lstFilePath, lstFileLastModified) +: resources.map { resource =>
          logger.debug(resource.getFile)
          val conn = resource.openConnection
          val lastModified = conn.getLastModified
          using(conn.getInputStream) { in =>
            if (pathLowerCase.endsWith(".min.js") || pathLowerCase.endsWith(".min.css")) {
              // 既に minifyされているものはそのまま出力
              org.apache.commons.io.IOUtils.copy(in, out, charset)
            } else if (pathLowerCase.endsWith(".js")) {
              modules.yuicompressor.compressJs(in, out, compressorConfig)
            } else if (pathLowerCase.endsWith(".css")) {
              modules.yuicompressor.compressCss(in, out, compressorConfig)
            } else {
              // jsでもcssでもないものはそのまま出力
              org.apache.commons.io.IOUtils.copy(in, out, charset)
            }
          }
          out.write('\n')
          (resource.getFile, lastModified)
        }
        out.flush

        Option(context.getResource(path)).map { resource =>
          // loadedFilesの中で一番新しいものをピックアップして実際に存在するファイルとタイムスタンプを比較、実際に存在するファイルのほうが古ければ警告を表示する
          val conn = resource.openConnection
          val lastModified = conn.getLastModified
          loadedFiles.sortBy(_._2).lastOption.foreach { case (latestFileName,latestFileLastModified) =>
            logger.debug("%s Last-Modified: %d".format(path, lastModified))
            logger.debug("%s Last-Modified: %d".format(latestFileName, latestFileLastModified))
            if (lastModified < latestFileLastModified) {
              logger.warn("File '%s' has been modified later than an actual file corresponging '%s'".format(latestFileName, path))
            }
          }
        }.getOrElse {
          logger.warn("An actual file corresponding '%s' is missing".format(path))
        }

      }.getOrElse { // 対応する .lstファイルが存在しない場合は本来の処理をする
        chain.doFilter(request, response)
      }
    }
    catch {
      case e:FileNotFoundException =>
        logger.error("Not Found: %s".format(e.getMessage))
        response.asInstanceOf[HttpServletResponse].sendError(HttpServletResponse.SC_NOT_FOUND, "'%s' specified in '%s' Not Found".format(e.getMessage, lstFilePath))
    }
  }

  override def destroy(): Unit = {}
}
