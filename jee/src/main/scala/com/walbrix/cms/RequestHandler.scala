package com.walbrix.cms

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.JsonNode
import com.typesafe.scalalogging.slf4j.LazyLogging
import com.walbrix.spring.{ScalaObjectMapper, ScalikeJdbcSupport}
import com.walbrix.spring.mvc._
import org.joda.time.DateTime
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.dao.DuplicateKeyException
import org.springframework.stereotype.Controller
import org.springframework.transaction.annotation.Transactional
import org.springframework.web.bind.annotation._
import org.springframework.web.multipart.MultipartFile
import scalikejdbc.WrappedResultSet

/**
 * Created by shimarin on 14/11/20.
 */
case class User(id:Int,username:String)
case object User {
  def apply(row:WrappedResultSet):User = User(row.int("id"),row.string("username"))
}

case class Entry(id:Int,prefix:String,name:String,title:String,description:Option[String],pageImage:Option[String],
                 content:Option[String],format:String,data:Option[String],visible:Boolean,publishedAt:Option[DateTime],
                 createdAt:DateTime,updatedAt:DateTime,labels:Seq[String] = Nil)

case object Entry {
  def apply(row:WrappedResultSet,detail:Boolean):Entry = {
    if (detail) {
      Entry(row.int("id"),row.string("prefix"),row.string("name"),row.string("title"),row.stringOpt("description"),row.stringOpt("page_image"),row.stringOpt("content"),row.string("format"),row.stringOpt("data"),row.boolean("visible"),row.jodaDateTimeOpt("published_at"),row.jodaDateTime("created_at"),row.jodaDateTime("updated_at"))
    } else {
      Entry(row.int("id"),row.string("prefix"),row.string("name"),row.string("title"),row.stringOpt("description"),row.stringOpt("page_image"),None,row.string("format"),row.stringOpt("data"),row.boolean("visible"),row.jodaDateTimeOpt("published_at"),row.jodaDateTime("created_at"),row.jodaDateTime("updated_at"))
    }
  }
}

case class Prefix(prefix:String,data:Option[String])

case class TransferOffer(entryId:Int,title:String,
                         sendUser:(Int,String), receiveUser:(Int,String),
                         description:Option[String],
                         createdAt:DateTime,updatedAt:DateTime)

trait Authentication extends com.walbrix.spring.mvc.Authentication[User,Int] with ScalikeJdbcSupport {
  override def checkPassword(username: String, password: String): Option[(Int, String)] = {
    val (userId,encrypted,authToken) =
      single(sql"select id,password,auth_token from users where username=${username}".map(row=>(row.int("id"),row.string("password"),row.string("auth_token")))).getOrElse(return None)
    if (com.walbrix.comparePassword(encrypted, password)) Some((userId,authToken)) else None
  }

  override def getUser(userId: Int): Option[User] = {
    single(sql"select * from users where id=${userId}".map(User(_)))
  }

  override def getIdFromUser(user: User): Int = user.id

  override def resetAuthToken(userId: Int): Option[String] = {
    string(sql"select username from users where id=${userId}").map { email =>
      val authToken = com.walbrix.generateAuthToken(email)
      update(sql"update users set auth_token=${authToken},auth_token_expires_at=DATEADD('MONTH', 6, now()) where id=${userId}")
      authToken
    }
  }

  override def getUserByAuthToken(authToken: String): Option[User] = {
    single(sql"select * from users where auth_token=${authToken}".map(User(_)))
  }
}

class AuthenticationBean extends com.walbrix.spring.mvc.AuthenticationBean[User,Int] with Authentication {
}

@Controller
@Transactional
@RequestMapping(Array(""))
class RequestHandler extends LoginRequestHandler[User,Int] with Authentication {
  @RequestMapping(value=Array("user"), method = Array(RequestMethod.GET))
  @ResponseBody
  def user():User = getUser()

  @RequestMapping(value=Array("user"), method = Array(RequestMethod.POST))
  @ResponseBody
  def saveUser(json:Map[String,Any]):User = {
    val user = getUser()
    val password = json("password").asInstanceOf[String]
    update(sql"update users set password=${com.walbrix.encryptPassword(password)} where id=${user.id}") // TODO:renew Auth Token
    getUser()  // TODO:RELOAD
  }

  @RequestMapping(value=Array("transfer"), method=Array(RequestMethod.GET))
  @ResponseBody
  def transferOffers():Seq[TransferOffer] = {
    val user = getUser()
    list(sql"""select transfer_offers.*,entries.*,
           U1.id as send_user_id,U1.username as send_username,
           U2.id as receive_user_id,U2.username as receive_username
           from transfer_offers,entries,users U1,users U2
           where transfer_offers.entry_id=entries.id
           and entries.user_id=U1.id
           and transfer_offers.receive_user_id=U2.id
           and ${user.id} in (U1.id,U2.id)
           order by updated_at desc""".map { row=>
      TransferOffer(row.int("transfer_offers.entry_id"),row.string("entries.title"),
        (row.int("send_user_id"),row.string("send_username")),
        (row.int("receive_user_id"),row.string("receive_username")),
        row.stringOpt("transfer_offers.description"),
        row.jodaDateTime("transfer_offers.created_at"),row.jodaDateTime("transfer_offers.updated_at"))
    })
  }

  @RequestMapping(value=Array("transfer/{entryId}"), method=Array(RequestMethod.POST))
  @ResponseBody
  def transferOffer(@PathVariable entryId:Int):Result[Nothing] = {
    val user = getUser()
    /*
    val offer = single(sql"""select * from transfer_offers,entries
                         where transfer_offers.entry_id=entries.id
                         and ${user.id} in (entries.user_id,receive_user_id)""".map { row=>
      (row.int("entries.user_id"), row.int("receive_user_id"))
    }).getOrElse(raiseNotFound)*/

    //println(list(sql"select * from transfer_offers".map(_.toMap())))

    if (update(sql"delete from transfer_offers where receive_user_id=${user.id} and entry_id=${entryId}") == 0) raiseNotFound
    val rst = update(sql"update entries set user_id=${user.id} where id=${entryId}")

    Result(rst > 0)
  }


  @RequestMapping(value=Array("signup"), method = Array(RequestMethod.POST),consumes=Array("application/json"))
  @ResponseBody
  def signup(@RequestBody json:Map[String,AnyRef]):Result[String] = {
    val username = json("username").asInstanceOf[String]
    val password = json("password").asInstanceOf[String]
    val authToken = com.walbrix.generateAuthToken(username)
    try {
      update(sql"""insert into users(username,password,auth_token,auth_token_expires_at)
             values(${username},${com.walbrix.encryptPassword(password)},${authToken},DATEADD('MONTH', 6, now()))""")
      Success()
    }
    catch {
      case e:DuplicateKeyException => Fail("ALREADYEXISTS")
    }
  }

  @RequestMapping(value=Array("dump.sql"), method=Array(RequestMethod.GET))
  def dump(request:HttpServletRequest, response:HttpServletResponse):Unit = {
    val remoteAddr = request.getRemoteAddr
    println(remoteAddr)
    if (!Array("127.0.0.1","::1","0:0:0:0:0:0:0:1").exists(_ == remoteAddr)) raiseForbidden
    response.setContentType("text/plain")
    val writer = response.getWriter
    list(sql"script".map(_.string(1))).foreach { line =>
      writer.println(line)
    }
    writer.flush()
  }
}

trait JacksonSupport {
  @Autowired private var objectMapper:ScalaObjectMapper = _

  def readTree(src:String):JsonNode = {
    objectMapper.readTree(src)
  }
}

@Controller
@Transactional
@RequestMapping(Array("entry"))
class EntryCRUDRequestHandler extends CRUDWithAuthentication[Entry,Int,User,Int] with Authentication with JacksonSupport with LazyLogging {
  override def create(entity: Entity, user: User): Option[Int] = {
    try {
      update(sql"insert into entries(user_id,prefix,name,title) values(${user.id},${entity.string("prefix")},${entity.string("name")},${entity.string("title")})")
      int(sql"select last_insert_id()")
    }
    catch {
      case e:DuplicateKeyException => None
    }
  }

  def update(id:Int, user:User, prefix:String,name:String,title:String,description:Option[String],pageImage:Option[String],
                      content:Option[String],format:String,data:Option[String],visible:Boolean,
                      publishedAt:Option[DateTime],labels:Iterable[String]):Boolean = {

    try {
      data.foreach { data =>
        val json = Option(data).map(readTree(_)).foreach { json =>
          if (!json.isObject) return false // check if json is valid
        }
      }
      update(sql"delete from entries_labels where entry_id=${id} and label not in (${labels})")
      labels.foreach { label =>
        update(sql"merge into entries_labels(entry_id,label) values(${id},${label})")
      }
      update(sql"""update entries set prefix=${prefix},name=${name},title=${title},description=${description},
        page_image=${pageImage},content=${content},format=${format},data=${data},visible=${visible},
        published_at=${publishedAt},updated_at=now() where id=${id} and user_id=${user.id}""") > 0
    }
    catch {
      case e:DuplicateKeyException => false
      case e:JsonProcessingException =>
        logger.debug("JsonProcessingException", e)
        false
    }
  }

  override def update(id: Int, entity: Entity, user: User): Boolean = {
    update(
      id, user,
      entity.string("prefix"),
      entity.string("name"),
      entity.string("title"),
      entity.stringOpt("description"),
      entity.stringOpt("pageImage"),
      entity.stringOpt("content"),
      entity.string("format"),
      entity.stringOpt("data"),
      entity.boolean("visible"),
      entity.jodaDateTimeOpt("publishedAt"),
      entity.get("labels").map(_.asInstanceOf[Iterable[String]]).getOrElse(Nil)
    )
  }

  @RequestMapping(value=Array("_st"), method=Array(RequestMethod.GET))
  @ResponseBody
  def get(@RequestParam start: Int, @RequestParam number: Int):Page[Entry] = {
    val user = getUser()
    Page(
      int(sql"select count(*) from entries where user_id=${user.id}").get,
      start,
      number,
      list(sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
        where user_id=${user.id} order by updated_at desc limit ${number} offset ${start}"""
        .one(Entry(_,false)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many)))
      )
  }

  override def get(offset: Int, limit: Int, ordering: Option[String], user: User): (Int, Seq[Entry]) = {
    (
      int(sql"select count(*) from entries where user_id=${user.id}").get,
      list(sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
        where user_id=${user.id} order by updated_at desc"""
        .one(Entry(_,false)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many)))
    )
  }

  override def get(id: Int, user: User): Option[Entry] = {
    single(sql"""select * from entries left outer join entries_labels on entries.id=entries_labels.entry_id
      where id=${id} and user_id=${user.id}"""
      .one(Entry(_,true)).toMany(_.stringOpt("label")).map((one,many) => one.copy(labels=many)))
  }

  override def delete(id: Int, user: User): Boolean = {
    update(sql"delete from entries where id=${id} and user_id=${user.id}") > 0
  }

  override def toIdType(id: String): Int = id.toInt

  @RequestMapping(value=Array("{entryId}/transfer"), method=Array(RequestMethod.POST))
  @ResponseBody
  def transferOffer(@PathVariable entryId:Int,@RequestBody json:Map[String,Any]):Result[String] = {
    val user = getUser()
    val receiveUserName = json.get("username").map(_.asInstanceOf[String]).getOrElse(raiseBadRequest("username missing"))
    val description = json.get("description").map(_.asInstanceOf[String])

    val confirmedEntryId = single(sql"select id from entries where id=${entryId} and user_id=${user.id}".map(_.int(1))).getOrElse(return Fail("No such entry"))
    val receiveUserId = single(sql"select id from users where username=${receiveUserName}".map(_.int(1))).getOrElse(return Fail("No such user"))

    val rst = update(sql"""merge into transfer_offers(entry_id,receive_user_id,description)
                 values(${confirmedEntryId},${receiveUserId},${description})""")
    Result(rst > 0)
  }

}

@Controller
@Transactional
@RequestMapping(Array("prefix"))
class PrefixCRUDRequestHandler extends CRUDWithAuthentication[Prefix,String,User,Int] with Authentication with JacksonSupport {
  override def create(entity: Entity, user: User): Option[String] = {
    val prefix = entity.string("prefix")
    try {
      update(sql"insert into prefixes(prefix,user_id) values(${prefix},${user.id})")
      Some(prefix)
    }
    catch {
      case e:DuplicateKeyException => None
    }
  }

  override def update(id: String, entity: Entity, user: User): Boolean = {
    val prefix = entity.string("prefix")
    val data = entity.stringOpt("data")
    try {
      data.foreach { data =>
        val json = Option(data).map(readTree(_)).foreach { json =>
          if (!json.isObject) return false // check if json is valid
        }
      }
      update(sql"update prefixes set prefix=${prefix},data=${data} where prefix=${id} and user_id=${user.id}") > 0
    }
    catch {
      case e:DuplicateKeyException => false
      case e:JsonProcessingException =>
        //println("json error")
        false
    }
  }

  override def get(offset: Int, limit: Int, ordering: Option[String], user: User): (Int, Seq[Prefix]) = {
    (
      int(sql"select count(*) from prefixes where user_id=${user.id}").get,
      list(sql"select * from prefixes where user_id=${user.id} order by prefix limit ${limit} offset ${offset}"
      .map(row=>Prefix(prefix=row.string("prefix"),data=row.stringOpt("data"))))
    )
  }

  override def get(id: String, user: User): Option[Prefix] = {
    single(sql"select * from prefixes where prefix=${id} and user_id=${user.id}"
      .map(row=>Prefix(prefix=row.string("prefix"),data=row.stringOpt("data"))))
  }

  override def delete(id: String, user: User): Boolean = {
    update(sql"delete from prefixes where prefix=${id} and user_id=${user.id}") > 0
  }

  override def toIdType(id: String): String = id.replaceAll("\\$", "/")
}

case class Media(id:Int,prefix:String,name:String,description:Option[String],contentType:String,hasPreview:Boolean,hasThumbnail:Boolean)

@Controller
@Transactional
@RequestMapping(Array("media"))
class MediaRequestHandler extends Authentication {
  @RequestMapping(value=Array(""), method = Array(RequestMethod.GET))
  @ResponseBody
  def get(@RequestParam(defaultValue="") prefix:String,
          @RequestParam(defaultValue="0") offset:Int,@RequestParam(defaultValue="20") limit:Int):Map[String,Any] = {
    val prefixCond = if (prefix != "") sqls" and (prefix=${prefix} or prefix like ${prefix + "/%"})" else sqls""
    val ordering = sqls"order by updated_at desc limit ${limit} offset ${offset}"
    val ownershipCond = sqls"and (user_id=${getUser().id} or user_id is null)"
    val count = int(sql"select count(*) from media where true ${ownershipCond} ${prefixCond}").get
    val items = list(sql"""select id,prefix,name,description,content_type,preview is not null as has_preview,thumbnail is not null as has_thumbnail
      from media where true ${ownershipCond} ${prefixCond} ${ordering}""".map { row =>
      Media(id=row.int("id"),prefix=row.string("prefix"),name=row.string("name"),description=row.stringOpt("description"),
        contentType=row.string("content_type"),hasPreview=row.boolean("has_preview"),hasThumbnail=row.boolean("has_thumbnail"))
    })
    Map("count"->count,"items"->items)
  }

  @RequestMapping(value=Array(""), method = Array(RequestMethod.POST))
  @ResponseBody
  def post(@RequestParam prefix:String,@RequestParam name:String,
           @RequestParam(required=false) description:String,@RequestParam file:MultipartFile):Result[Int] = {
    val rst = update(sql"merge into media(prefix,name,description,user_id,content_type,content) key(prefix,name) values(${prefix},${name},${description},${getUser().id},${file.getContentType},${file.getInputStream})")
    // TODO: create preview, thumbnail
    // http://qiita.com/dahugani/items/8c54247caecfb0279e75
    Result(rst > 0)
  }

  @RequestMapping(value=Array("{id}"), method = Array(RequestMethod.DELETE))
  @ResponseBody
  def delete(@PathVariable id:Int):Result[Nothing] = {
    Result(update(sql"delete from media where id=${id} and user_id=${getUser().id}") > 0)
  }
}
