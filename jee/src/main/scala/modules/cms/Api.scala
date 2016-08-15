package modules.cms

import scalikejdbc.DB
import org.scalatra.{UnsupportedMediaType,BadRequest,Ok}
import modules.scalatra.{Success,Failure}

case class User(id:Int,name:String)

class Api extends modules.scalatra.JsonSupport with modules.scalatra.LogErrors {
  get("/") {
    Map[String,Any](
      "version"->buildinfo.BuildInfo.version
    )
  }

  def getCurrentUserId:Option[Int] = enrichSession(session).getAs[Int]("userId")

  get("/login") {
    getCurrentUserId match {
      case Some(userId) => Success(User(userId, "shimarin"))
      case None => Failure
    }
  }

  post("/login") {
    val userId = 1
    session.setAttribute("userId", userId)
    Success(userId)
  }

  post("/logout") {
    getCurrentUserId match {
      case Some(userId) =>
        session.removeAttribute("userId")
        Success(userId)
      case None => Failure
    }
  }

  def withUser[T](f:User=>T):T = {
    val userId = getCurrentUserId.getOrElse(halt(403, "You must be authenticated"))
    val user = DB readOnly { implicit session =>
      val name = sql"select name from users where id=${userId}".map(_.string(1)).single.apply.getOrElse(halt(403, "User does not exist (deleted?)"))
      User(userId, name)
    }
    f(user)
  }

  post("/image/:sizespec?") {
    import modules.image._
    withUser { user =>
      val requestContentType = request.getContentType
      if (!requestContentType.startsWith("image/")) {
        halt(UnsupportedMediaType("image/* only"))
      }

      try {
        val (image, format) = resizeImage(request.getInputStream, params.get("sizespec"))
        Ok(toDataURI(format, image), Map("Content-Type" -> ("text/plain")))
      }
      catch {
        case e:IllegalArgumentException => BadRequest("Invalid image file")
      }
    }
  }
}
