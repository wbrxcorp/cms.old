package modules.wikipedia

case class Canonical(url:String, pageName:Option[String])

object Canonical {
  private val urlPattern = """^http:\/\/ja\.wikipedia\.org\/wiki\/(.+)$""".r

  def apply(url:String):Canonical = {
    Canonical(url, url match {
      case urlPattern(x) => Some(java.net.URLDecoder.decode(x, "UTF-8"))
      case _ => None
    })
  }
}
