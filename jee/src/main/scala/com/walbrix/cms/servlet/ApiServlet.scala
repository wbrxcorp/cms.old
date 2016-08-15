package com.walbrix.cms.servlet

import org.json4s.JObject
import org.scalatra.BadRequest
import scalikejdbc.{DB,DBSession,WrappedResultSet}
import org.joda.time.DateTime

trait JsonSupport extends org.scalatra.ScalatraServlet with org.scalatra.json.JacksonJsonSupport with com.typesafe.scalalogging.slf4j.LazyLogging {
  override protected implicit def jsonFormats: org.json4s.Formats = org.json4s.DefaultFormats.withBigDecimal ++ org.json4s.ext.JodaTimeSerializers.all
  def withJsonObject[T](obj:org.json4s.JValue)(f:JObject=>T):T = obj match {
    case x:JObject =>
      logger.debug(x.toString)
      try {
        f(x)
      }
      catch {
        case e:Throwable =>
          logger.debug("error parsing json object", e)
          halt(BadRequest(e))
      }
    case _ => halt(BadRequest("Request body must be a json object"))
  }

  def withJsonRequest[T](f:JObject=>T):T = withJsonObject(parsedBody)(f)
}

trait AuthSupport {
  private def getSha256(str:String):String = {
    val buf = new StringBuffer()
    val md = java.security.MessageDigest.getInstance("SHA-256")
    md.update(str.getBytes())
    md.digest().foreach { b =>
      buf.append("%02x".format(b))
    }
    buf.toString
  }

  def comparePassword(encrypted:String, password:String):Boolean = {
    encrypted.split('$') match {
      case x if x.length == 2 => x(1) == getSha256("%s%s".format(x(0), password))
      case _ => false
    }
  }

  def encryptPassword(password:String):String = {
    val salt = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).mkString
    "%s$%s".format(salt, getSha256("%s%s".format(salt, password)))
  }


  def generateAuthToken(id:String):String =
    "%s%s".format(getSha256(id), new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(16).mkString)
}

case class Login(username:String, password:String)
case class Result(success:Boolean, info:Option[Any]=None)

case class User(id:Int,username:String)
object User {
  def apply(row:WrappedResultSet):User = User(row.int("id"),row.string("username"))
}

case class Entry(id:Int,prefix:String,name:String,title:String,description:Option[String],pageImage:Option[String],
                 content:Option[String],format:String,data:Option[String],visible:Boolean,publishedAt:Option[DateTime],
                 createdAt:DateTime,updatedAt:DateTime,labels:Seq[String] = Nil)

object Entry {
  def apply(row:WrappedResultSet,detail:Boolean):Entry = {
    if (detail) {
      Entry(row.int("id"),row.string("prefix"),row.string("name"),row.string("title"),row.stringOpt("description"),row.stringOpt("page_image"),row.stringOpt("content"),row.string("format"),row.stringOpt("data"),row.boolean("visible"),row.jodaDateTimeOpt("published_at"),row.jodaDateTime("created_at"),row.jodaDateTime("updated_at"))
    } else {
      Entry(row.int("id"),row.string("prefix"),row.string("name"),row.string("title"),row.stringOpt("description"),row.stringOpt("page_image"),None,row.string("format"),row.stringOpt("data"),row.boolean("visible"),row.jodaDateTimeOpt("published_at"),row.jodaDateTime("created_at"),row.jodaDateTime("updated_at"))
    }
  }
}

case class Prefix(prefix:String,data:Option[String])
object Prefix {
  def apply(row:WrappedResultSet):Prefix = Prefix(prefix=row.string("prefix"),data=row.stringOpt("data"))
}

case class TransferOffer(entryId:Int,title:String,
                         sendUser:(Int,String), receiveUser:(Int,String),
                         description:Option[String],
                         createdAt:DateTime,updatedAt:DateTime)
object TransferOffer {
  def apply(row:WrappedResultSet):TransferOffer = TransferOffer(
    row.int("transfer_offers.entry_id"),row.string("entries.title"),
    (row.int("send_user_id"),row.string("send_username")),
    (row.int("receive_user_id"),row.string("receive_username")),
    row.stringOpt("transfer_offers.description"),
    row.jodaDateTime("transfer_offers.created_at"),row.jodaDateTime("transfer_offers.updated_at"))
}

case class Page[T](count:Int,offset:Int,limit:Int,items:Seq[T])

class ApiServlet extends JsonSupport with AuthSupport with scalikejdbc.SQLInterpolation {
  post("/login") {
    val login = withJsonRequest(_.extract[Login])

    DB readOnly { implicit session =>
      sql"select id,password from users where username=${login.username}".map(row=>(row.int("id"),row.string("password"))).single.apply
    } match {
      case Some((userId,password)) if comparePassword(password, login.password) =>
        session.setAttribute("user_id", userId)
        Result(true)
      case _ => Result(false)
    }
  }

  post("/logout") {
    session.removeAttribute("user_id")
    Result(true)
  }

  def getUser(dbsession:DBSession):User = {
    enrichSession(session).getAs[Int]("user_id").map { userId =>
      implicit val session = dbsession
      sql"select id,username from users where id=${userId}".map(User(_)).single.apply
    }.flatten.getOrElse(halt(org.scalatra.Forbidden()))

  }

  def getUser:User = DB readOnly { session => getUser(session) }

  def list[T](defaultLimit:Int=20,defaultOrdering:String="")(f:(Int,Int,String)=>(Int,Seq[T])):Page[T] = {
    val (offset, limit, ordering) = (
      params.getAs[Int]("offset").getOrElse(0),
      params.getAs[Int]("limit").getOrElse(defaultLimit),
      params.getAs[String]("ordering").getOrElse(defaultOrdering)
    )
    val (count, items) = f(offset, limit, ordering)
    Page(count, offset, limit, items)
  }

  def get[T]()(f:Unit=>Option[T]):T = {
    f().getOrElse(halt(org.scalatra.NotFound()))
  }

  get("/user") { getUser }

  // entry operations
  get("/entry") {
    list() { case (offset, limit, ordering) =>
      DB readOnly { implicit session =>
        val userId = getUser(session).id
        (
          sql"select count(*) from entries where user_id=${userId}".map(_.int(1)).single.apply.get,
          sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
            where user_id=${userId} order by updated_at desc"""
            .one(Entry(_,false)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many)).list.apply
        )
      }
    }
  }

  get("/entry/:id") {
    val entryId = params.getAs[Int]("id").getOrElse(halt(org.scalatra.BadRequest()))
    (DB readOnly { implicit session =>
      val userId = getUser(session).id
      sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
        where id=${entryId} and user_id=${userId}"""
        .one(Entry(_,true)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many)).single.apply
    }).getOrElse(halt(org.scalatra.NotFound()))
  }

  // prefix operations
  get("/prefix") {
    list() { case (offset, limit, ordering) =>
      DB readOnly { implicit session =>
        val userId = getUser(session).id
        (
          sql"select count(*) from prefixes where user_id=${userId}".map(_.int(1)).single.apply.get,
          sql"select * from prefixes where user_id=${userId} order by prefix limit ${limit} offset ${offset}".map(Prefix(_)).list.apply
        )
      }
    }
  }

  // transfer operations
  get("/transfer") {
    list() { case (offset, limit, ordering) =>
      DB readOnly { implicit session =>
        val userId = getUser(session).id
        (
          sql"select count(*) from transfer_offers,entries,users U1,users U2 where transfer_offers.entry_id=entries.id and entries.user_id=U1.id and transfer_offers.receive_user_id=U2.id and ${userId} in (U1.id,U2.id)".map(_.int(1)).single.apply.get,
          sql"""select transfer_offers.*,entries.*,
                 U1.id as send_user_id,U1.username as send_username,
                 U2.id as receive_user_id,U2.username as receive_username
                 from transfer_offers,entries,users U1,users U2
                 where transfer_offers.entry_id=entries.id
                 and entries.user_id=U1.id
                 and transfer_offers.receive_user_id=U2.id
                 and ${userId} in (U1.id,U2.id)
                 order by updated_at desc limit ${limit} offset ${offset}""".map(TransferOffer(_)).list.apply
        )
      }
    }
  }

}
