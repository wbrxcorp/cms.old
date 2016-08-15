package com.walbrix.cms.servlet

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.OutputStream
import java.sql.Blob
import javax.imageio.ImageIO
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import com.fasterxml.jackson.databind.ObjectMapper
import com.walbrix.spring.ScalikeJdbcSupport
import org.apache.commons.io.IOUtils
import org.apache.commons.io.output.NullOutputStream
import org.joda.time.DateTime
import org.pegdown.{Extensions, PegDownProcessor}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.transaction.annotation.Transactional
import org.springframework.web.context.support.WebApplicationContextUtils
import scalikejdbc.{DB,DBSession,WrappedResultSet}
import org.json4s.{JValue,JsonDSL,Extraction}
import org.json4s.jackson.JsonMethods

/**
 * Created by shimarin on 14/11/20.
 */

abstract class HttpServletBean {
  private var servlet:HttpServlet = _
  private var lastModifiedFunc:HttpServletRequest=>Long = _

  def setServlet(servlet:HttpServlet):Unit = this.servlet = servlet
  def setLastModifiedFunc(lastModifiedFunc:HttpServletRequest=>Long):Unit = this.lastModifiedFunc = lastModifiedFunc

  protected def getLastModified(request:HttpServletRequest):Long = lastModifiedFunc(request)

  def delete(request:HttpServletRequest, response:HttpServletResponse):Unit
  def get(request:HttpServletRequest, response:HttpServletResponse):Unit
  def head(request:HttpServletRequest, response:HttpServletResponse):Unit
  def options(request:HttpServletRequest, response:HttpServletResponse):Unit
  def post(request:HttpServletRequest, response:HttpServletResponse):Unit
  def put(request:HttpServletRequest, response:HttpServletResponse):Unit
  def trace(request:HttpServletRequest, response:HttpServletResponse):Unit
}

class BeanServlet extends HttpServlet {
  private var handler:HttpServletBean = _

  override def init():Unit = {
    val applicationContext = WebApplicationContextUtils.getWebApplicationContext(getServletContext).getAutowireCapableBeanFactory
    handler = applicationContext.getBean(this.getServletName).asInstanceOf[HttpServletBean]
    handler.setLastModifiedFunc(this.getLastModified)
  }

  override def doDelete(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    handler.delete(request, response)
  }
  override def doGet(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    handler.get(request, response)
  }
  override def doHead(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    handler.head(request, response)
  }
  override def doOptions(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    handler.options(request, response)
  }
  override def doPost(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    handler.post(request, response)
  }
  override def doPut(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    handler.put(request, response)
  }
  override def doTrace(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    handler.trace(request, response)
  }
}

@Transactional
class RestServlet extends HttpServletBean {
  @Autowired private var objectMapper:ObjectMapper = _

  private class HttpErrorException(val code:Int,reason:String) extends Exception(reason) {}

  protected def raiseNotFound:Nothing = throw new HttpErrorException(HttpServletResponse.SC_NOT_FOUND, null)
  protected def raiseNotFound(reason:String):Nothing = throw new HttpErrorException(HttpServletResponse.SC_NOT_FOUND, reason)
  protected def raiseForbidden:Nothing = throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, null)
  protected def raiseForbidden(reason:String):Nothing = throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, reason)
  protected def raiseBadRequest:Nothing = throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, null)
  protected def raiseBadRequest(reason:String):Nothing = throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, reason)
  protected def raiseMethodNotAllowed = throw new HttpErrorException(HttpServletResponse.SC_METHOD_NOT_ALLOWED, null)

  protected def parseJson(json:String):Map[String,Any] = {
    objectMapper.readValue(json, classOf[Map[String,Any]]).asInstanceOf[Map[String,Any]]
  }

  private def processRequest(func:(HttpServletRequest,ResponseHeader)=>AnyRef, request:HttpServletRequest, response:HttpServletResponse,
                              useOutputStream:Option[OutputStream] = None):Unit = {
    val header = new ResponseHeader(response)
    try {
      func(request, header) match {
        case result:StreamResponse =>
          response.setContentType(result.contentType)
          result.func(useOutputStream.getOrElse(response.getOutputStream))
        case result:StringResponse =>
          response.setContentType(result.contentType)
          val bytes = result.content.getBytes(result.charset)
          response.setContentLength(bytes.length)
          response.getOutputStream.write(bytes)
        case result =>
          response.setContentType("application/json")
          objectMapper.writeValue(response.getOutputStream, result)
      }

    }
    catch {
      case e:HttpErrorException =>
        response.setStatus(e.code)
        response.setContentType("application/json")
        objectMapper.writeValue(useOutputStream.getOrElse(response.getOutputStream),
          Map("success"->false,"info"->Map("code"->e.code,"mesasge"->e.getMessage)))
    }
  }

  class ResponseHeader(response:HttpServletResponse) {
    def setHeader(name:String,value:String):Unit = response.setHeader(name, value)
  }

  case class StreamResponse(contentType:String,func:OutputStream=>Unit)
  object StreamResponse {
    def apply(contentType:String)(func:OutputStream=>Unit)(implicit d1: DummyImplicit):StreamResponse =
      StreamResponse(contentType, func)
  }

  case class StringResponse(contentType:String,content:String,charset:String="UTF-8")

  def delete(request:HttpServletRequest,header:ResponseHeader):AnyRef = raiseMethodNotAllowed
  override def delete(request:HttpServletRequest, response:HttpServletResponse):Unit =
    processRequest(delete, request, response)

  def get(request:HttpServletRequest,header:ResponseHeader):AnyRef = raiseMethodNotAllowed
  override def get(request:HttpServletRequest, response:HttpServletResponse):Unit =
    processRequest(get, request, response)

  def head(request:HttpServletRequest,header:ResponseHeader):AnyRef = {
    get(request, header)
  }
  override def head(request:HttpServletRequest, response:HttpServletResponse):Unit =
    processRequest(head, request, response, Some(new NullOutputStream))

  def options(request:HttpServletRequest,header:ResponseHeader):AnyRef = raiseMethodNotAllowed
  override def options(request:HttpServletRequest, response:HttpServletResponse):Unit =
    processRequest(options, request, response)

  def post(request:HttpServletRequest,header:ResponseHeader):AnyRef = raiseMethodNotAllowed
  override def post(request:HttpServletRequest, response:HttpServletResponse):Unit =
    processRequest(post, request, response)

  def put(request:HttpServletRequest,header:ResponseHeader):AnyRef = raiseMethodNotAllowed
  override def put(request:HttpServletRequest, response:HttpServletResponse):Unit =
    processRequest(put, request, response)

  def trace(request:HttpServletRequest,header:ResponseHeader):AnyRef = raiseMethodNotAllowed
  override def trace(request:HttpServletRequest, response:HttpServletResponse):Unit =
    processRequest(trace, request, response)

}

class ContentsServlet extends RestServlet with ScalikeJdbcSupport {

  val defaultLimit = 20

  def getEntryDetail(row:WrappedResultSet):scala.collection.mutable.Map[String,Any] = {
    val entry = scala.collection.mutable.Map[String,Any]()
    row.stringOpt("data").foreach { data =>
      parseJson(data).foreach(entry += _)
    }
    entry += (
      "prefix"->row.string("prefix"),
      "name"->row.string("name"),
      "title"->row.string("title"),
      "description"->row.stringOpt("description"),
      "page_image"->row.stringOpt("page_image"),
      "content"->row.stringOpt("content"),
      "format"->row.string("format"),
      "published_at"->row.jodaDateTimeOpt("published_at"),
      "updated_at"->row.jodaDateTime("entries.updated_at"),
      "username"->row.string("username")
      )
    entry
  }

  private def updatePrefixData(prefix:String, entry:scala.collection.mutable.Map[String,Any]):Unit = {
    val buf = new StringBuilder()
    prefix.split('/').foreach { pathElement =>
      if (buf.length > 0) buf.append('/')
      buf.append(pathElement)
      single(sql"select data from prefixes where prefix=${buf.toString}".map(_.stringOpt("data"))).flatten.foreach { data =>
        parseJson(data).foreach(entry += _)
      }
    }
  }

  def get(prefix:String, name:String):scala.collection.mutable.Map[String,Any] = {
    val entry = scala.collection.mutable.Map[String,Any]()

    updatePrefixData(prefix, entry)

    entry ++ single(sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
                 left join users on entries.user_id=users.id
      where user_id=users.id and prefix=${prefix} and name=${name} and visible"""
      .one(getEntryDetail(_)).toMany(_.stringOpt("label")).map((one,many) => one + ("labels"->many))).getOrElse(raiseNotFound(prefix + "/" + name + ".json"))
  }

  def html(prefix:String,name:String):StringResponse = {
    val (content, format) = single(sql"select content,format from entries where prefix=${prefix} and name=${name}"
      .map(row=>(row.stringOpt("content"),row.string("format")))).get
    val contentHtml = content.map { content =>
      format match {
        case "markdown" => new PegDownProcessor(Extensions.ALL).markdownToHtml(content)
        case "html" => content
        case _ => "UNKNOWN CONTENT FORMAT:%s".format(format)
      }
    }.getOrElse("")
    StringResponse("text/html;charset=UTF-8", contentHtml)
  }

  def entryConditions(labels:Seq[String]):SQLSyntax = {
    val baseConditions = sqls"published_at is not null and published_at <= now() and visible"
    labels match {
      case labels if labels.length > 0 =>
        baseConditions.append(sqls" and exists(select * from entries_labels where entry_id=entries.id and label in (${labels}))")
      case _ =>
        baseConditions
    }
  }

  def prefixCondition(prefix:String):SQLSyntax = {
    if (prefix != "") sqls"(prefix=${prefix} or prefix like ${prefix + "/%"})" else sqls"true"
  }

  def entries(prefix:String,offset:Int,limit:Int,labels:Seq[String]):Seq[Entry] = {
    val prefixCond = prefixCondition(prefix)
    if (int(sql"select count(*) from entries where ${prefixCond}").get == 0) {
      raiseNotFound(prefix)
    }

    val sql = sql"""select prefix,name,title,description,page_image,format,published_at,entries.updated_at,username,label
                 from entries left outer join entries_labels on entries.id=entries_labels.entry_id
                 left join users on entries.user_id=users.id
                 where ${entryConditions(labels)}
                 and ${prefixCond}
                 order by published_at desc limit ${limit} offset ${offset}"""
          .one(Entry(_, false)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many))
    list(sql)
  }

  def byMonth(prefix:String,labels:Seq[String]):Seq[(Int,Int,Int)] = {
    val prefixCond = prefixCondition(prefix)
    //create local temporary table hoge on commit drop transactional as select year(published_at) as y,month(published_at) as m FROM entries where published_at is not null group by y,m order by y desc,m desc;
    //select y,m,count(y*100+m) from hoge,entries where y=year(published_at) and m=month(published_at) group by y,m order by y desc,m desc;
    Seq()  // TODO: Implement
  }

  def byLabel(prefix:String):Seq[(String,Int)] = {
    val prefixCond = prefixCondition(prefix)
    Seq()  // TODO: Implement
  }

  def list(prefix:String,offset:Int,limit:Int,labels:Seq[String]):Map[String,Any] = {
    val entry = scala.collection.mutable.Map[String,Any]()
    updatePrefixData(prefix, entry)
    entry += "entries"->entries(prefix,offset,limit,labels)
    entry += "by_month"->byMonth(prefix,labels)
    entry += "by_label"->byLabel(prefix)
    entry.map(kv => (kv._1,kv._2)).toMap
  }

  def getMedia(prefix:String,name:String):(String,Blob) = {
    single(sql"select content_type,content from media where prefix=${prefix} and name=${name}".map { row =>
      (row.string("content_type"), row.blob("content"))
    }).getOrElse(raiseNotFound(prefix + "/" + name))
  }

  def parsePathInfo(rawPathInfo:String):(String,Option[String]) = {
    val pathInfo = Option(rawPathInfo).map(_.replaceFirst("^\\/+", "")).getOrElse("")

    val lastSlashPos = pathInfo.lastIndexOf('/')
    val prefix = if (lastSlashPos >= 0) pathInfo.substring(0, lastSlashPos) else ""
    val name = (if (lastSlashPos >= 0) pathInfo.substring(lastSlashPos + 1) else pathInfo) match { case "" => None case x => Some(x)}
    (prefix,name)
  }

  private def processImage(contentType:String,blob:Blob, size:Option[Int]):StreamResponse = {
    StreamResponse(contentType) { out =>
      try {
        size match {
          case Some(size) if size > 0 && size < 512 =>
            val original = ImageIO.read(blob.getBinaryStream)
            // todo prohibiting expanding size
            val bufferedImage = new BufferedImage(size, size, original.getType)
            val scaled = original.getScaledInstance(size, size, Image.SCALE_AREA_AVERAGING)
            bufferedImage.getGraphics.drawImage(scaled, 0, 0, size, size, null)
            ImageIO.write(bufferedImage, contentType.split('/')(1), out)
          case _ =>
            IOUtils.copy(blob.getBinaryStream, out)
        }
      }
      finally {
        blob.free()
      }
    }
  }

  override def get(request: HttpServletRequest, header:ResponseHeader): AnyRef = {
    header.setHeader("Access-Control-Allow-Origin","*")
    parsePathInfo(request.getPathInfo) match {
      case (prefix, Some(name)) if (name.endsWith(".json")) =>
        get(prefix, name.replaceFirst("\\.json$",""))
      case (prefix, Some(name)) if (name.endsWith(".html")) =>
        html(prefix, name.replaceFirst("\\.html$",""))
      case (prefix, Some(name)) =>
        val (contentType,blob) = getMedia(prefix, name)
        contentType match {
          case "image/png" | "image/jpeg" => processImage(contentType, blob, Option(request.getParameter("size")).map(_.toInt))
          case _ =>
            StreamResponse(contentType) { out =>
              try {
                IOUtils.copy(blob.getBinaryStream, out)
              }
              finally {
                blob.free()
              }
            }
        }
      case (prefix, None) => // プレフィクスに該当するエントリの一覧（さらに各条件）
        val offset = Option(request.getParameter("offset")).map(_.toInt).getOrElse(0)
        val limit = Option(request.getParameter("limit")).map(_.toInt).getOrElse(defaultLimit)
        val label = Option(request.getParameterValues("label")).map(_.toSeq).getOrElse(Nil)
        list(prefix,offset,limit, label)
    }
  }
}

class ContentsScalatraServlet extends org.scalatra.ScalatraServlet with scalikejdbc.SQLInterpolation {

  val defaultLimit = 20
  implicit def jsonFormats: org.json4s.Formats = org.json4s.DefaultFormats.withBigDecimal ++ org.json4s.ext.JodaTimeSerializers.all

  def parseJson(str:String):JValue = JsonMethods.parse(str)

  def entryConditions(labels:Seq[String]):SQLSyntax = {
    val baseConditions = sqls"published_at is not null and published_at <= now() and visible"
    labels match {
      case labels if labels.length > 0 =>
        baseConditions.append(sqls" and exists(select * from entries_labels where entry_id=entries.id and label in (${labels}))")
      case _ =>
        baseConditions
    }
  }

  def prefixCondition(prefix:String):SQLSyntax = {
    prefix match {
      case "" => sqls"true"
      case _ => sqls"(prefix=${prefix} or prefix like ${prefix + "/%"})"
    }
  }
/*
  def get(prefix:String, name:String):scala.collection.mutable.Map[String,Any] = {
    val entry = scala.collection.mutable.Map[String,Any]()

    updatePrefixData(prefix, entry)

    entry ++ single(sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
                 left join users on entries.user_id=users.id
      where user_id=users.id and prefix=${prefix} and name=${name} and visible"""
      .one(getEntryDetail(_)).toMany(_.stringOpt("label")).map((one,many) => one + ("labels"->many))).getOrElse(raiseNotFound(prefix + "/" + name + ".json"))
  }

  def html(prefix:String,name:String):StringResponse = {
    val (content, format) = single(sql"select content,format from entries where prefix=${prefix} and name=${name}"
      .map(row=>(row.stringOpt("content"),row.string("format")))).get
    val contentHtml = content.map { content =>
      format match {
        case "markdown" => new PegDownProcessor(Extensions.ALL).markdownToHtml(content)
        case "html" => content
        case _ => "UNKNOWN CONTENT FORMAT:%s".format(format)
      }
    }.getOrElse("")
    StringResponse("text/html;charset=UTF-8", contentHtml)
  }
*/
  def getPrefixData(prefix:String)(implicit session:DBSession):JValue = {
    // プレフィクスのデータを得る（上位階層から下位階層に向かってマージ）
    // a/b/c/d -> Seq(a,a/b,a/b/c,a/b/c/d)
    val prefixes = prefix.split('/').foldLeft(Seq("")) { case (a, b) => a.last match { case "" => a :+ b case last => a :+ (last + '/' + b) } }
    // 各prefixのデータを先に読み込んでmapにしておく
    val prefixDataMap = sql"select prefix,data from prefixes where prefix in (${prefixes})".map { row =>
      (row.string(1),parseJson(row.string(2)))
    }.list.apply.toMap
    // data: prefixの上の階層から下に向かってprefix dataをマージしていく
    prefixes.foldLeft(parseJson("{}")) { case (data, prefix) =>
      prefixDataMap.get(prefix) match {
        case Some(pdata) => data merge pdata
        case None => data
      }
    }
  }

  def getEntry(prefix:String, name:String)(implicit session:DBSession):(Entry,JValue) = {
    val entry = sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
                 left join users on entries.user_id=users.id
      where user_id=users.id and prefix=${prefix} and name=${name} and visible"""
      .one(Entry(_, true)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many)).single.apply.getOrElse(halt(org.scalatra.NotFound()))

    (entry, getPrefixData(prefix))
  }

  // プレフィクスに対するリクエスト
  get("/*") {
    response.setHeader("Access-Control-Allow-Origin","*")
    val prefix = multiParams("splat").head.replaceAll("/+$", "")

    val (offset, limit, labels) = (
      params.getAs[Int]("offset").getOrElse(0),
      params.getAs[Int]("limit").getOrElse(defaultLimit),
      multiParams.getAs[String]("label").getOrElse(Nil)
    )

    // プレフィクスのデータを得る（上位階層から下位階層に向かってマージ）
    // a/b/c/d -> Seq(a,a/b,a/b/c,a/b/c/d)
    val prefixes = prefix.split('/').foldLeft(Seq("")) { case (a, b) => a.last match { case "" => a :+ b case last => a :+ (last + '/' + b) } }

    val (data:org.json4s.JValue, entries) = DB readOnly { implicit session =>
      // 指定のprefix登録がなく、かつ該当するエントリがひとつもない場合は404にする
      val prefixCond = prefixCondition(prefix)  // prefixのマッチ条件SQL
      sql"select (select count(*) from entries where ${prefixCond}) + (select count(*) from prefixes where prefix=${prefix})".map(_.int(1)).single.apply.filter(_ != 0).getOrElse(halt(org.scalatra.NotFound()))

      // 各prefixのデータを先に読み込んでmapにしておく
      val prefixDataMap = sql"select prefix,data from prefixes where prefix in (${prefixes})".map { row =>
        (row.string(1),parseJson(row.string(2)))
      }.list.apply.toMap

      (
        // data: prefixの上の階層から下に向かってprefix dataをマージしていく
        prefixes.foldLeft(parseJson("{}")) { case (data, prefix) =>
          prefixDataMap.get(prefix) match {
            case Some(pdata) => data merge pdata
            case None => data
          }
        },
        // entries: 指定のprefixとlabelsに該当するエントリ
        sql"""select entries.id as id,prefix,name,title,description,page_image,format,published_at,data,visible,entries.created_at,entries.updated_at,username,label
                     from entries left outer join entries_labels on entries.id=entries_labels.entry_id
                     left join users on entries.user_id=users.id
                     where ${entryConditions(labels)}
                     and ${prefixCond}
                     order by published_at desc limit ${limit} offset ${offset}"""
              .one(Entry(_, false)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many)).list.apply
      )

    }

    val json = data merge JsonDSL.pair2jvalue("entries"->Extraction.decompose(entries).snakizeKeys)
    org.scalatra.Ok(JsonMethods.compact(json), Map(("content-type"->"application/json")))
  }

  // コンテンツに対するリクエスト
  get("/*.*") {
    response.setHeader("Access-Control-Allow-Origin","*")
    val (prefix, name, suffix) = multiParams("splat") match { case splat =>
      val (prefix, name) = splat(0).replaceAll("/+","/").replaceAll("^/", "") match { case path =>
        path.lastIndexOf('/') match {
          case -1 => ("", path)
          case lastSlashPos => (path.substring(0, lastSlashPos), path.substring(lastSlashPos + 1) )
        }
      }
      (prefix,name, splat(1))
    }

    val (entry, data) = DB readOnly { implicit session => getEntry(prefix, name) }

    (suffix, entry.format) match {
      case ("json", _) =>
        org.scalatra.Ok(JsonMethods.compact(data merge Extraction.decompose(entry).snakizeKeys), Map(("content-type"->"application/json")))
      case ("html", "markdown") =>
        org.scalatra.Ok("<script>window.cms_data = %s</script>\n".format(JsonMethods.compact(data merge JsonDSL.pair2jvalue("entry"->Extraction.decompose(entry).snakizeKeys))) + new PegDownProcessor(Extensions.ALL).markdownToHtml(entry.content.getOrElse("")), Map(("content-type"->"text/html;charset=UTF-8")))
      case ("html", "html") =>
        org.scalatra.Ok("<script>window.cms_data = %s</script>\n".format(JsonMethods.compact(data merge JsonDSL.pair2jvalue("entry"->Extraction.decompose(entry).snakizeKeys))) + entry.content.getOrElse(""), Map(("content-type"->"text/html;charset=UTF-8")))
      case _ => org.scalatra.UnsupportedMediaType()
    }
  }
}
