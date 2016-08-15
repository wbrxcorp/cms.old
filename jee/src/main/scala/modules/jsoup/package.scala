package modules

import java.net.URL
import scala.language.implicitConversions
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.parser.Parser

package object jsoup {
  val defaultCharset:String = "UTF-8"
  def jsoupFromFile(in:java.io.File, baseUri:Option[String]=None, charset:String=defaultCharset):Document = {
    baseUri match {
      case Some(baseUri) => Jsoup.parse(in, charset, baseUri)
      case None => Jsoup.parse(in, charset)
    }
  }

  def jsoupFromStream(in:java.io.InputStream, baseUri:String, parser:Option[Parser]=None, charset:String=defaultCharset):Document = {
    parser match {
      case Some(parser) => Jsoup.parse(in, charset, baseUri, parser)
      case None => Jsoup.parse(in, charset, baseUri)
    }
  }

  def jsoupFromString(html:String, baseUri:Option[String]=None, parser:Option[Parser]=None):Document = {
    baseUri match {
      case Some(baseUri) => parser match {
        case Some(parser) => Jsoup.parse(html, baseUri, parser)
        case None => Jsoup.parse(html, baseUri)
      }
      case None => Jsoup.parse(html)
    }
  }

  implicit def string2url(url:String):Either[URL,String] = Right(url)
  implicit def url2url(url:URL):Either[URL,String] = Left(url)
  def jsoupFromURL(url:Either[URL,String], timeoutMillis:Int = 30000):Document = {
    Jsoup.parse(url match { case Left(url)=>url case Right(url) => new URL(url) }, timeoutMillis)
  }
}
