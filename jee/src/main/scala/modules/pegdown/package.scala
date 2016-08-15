package modules

import org.pegdown.LinkRenderer

package object pegdown {
  def renderMarkdown(markdown:String, linkRenderer:Option[LinkRenderer]=None):String = {
    val processor = new org.pegdown.PegDownProcessor(org.pegdown.Extensions.ALL)
    linkRenderer match {
      case Some(linkRenderer) => processor.markdownToHtml(markdown, linkRenderer)
      case None => processor.markdownToHtml(markdown)
    }
  }

  def renderMarkdown(markdown:String, linkRenderer:LinkRenderer):String = renderMarkdown(markdown, Some(linkRenderer))

  val markdownMetadataRegex ="""^(.+:.+\n)+\n""".r

  def extractMetadataFromMarkdown(md:String):(String,Map[String,String]) = {
    markdownMetadataRegex.findFirstIn(md) match {
      case Some(metadata) =>
        val metamap = """(?m)^(.+:.+)$""".r.findAllIn(metadata).map { single =>
          val splitted = single.split(":")
          (splitted(0).trim, splitted(1).trim)
        }.toMap
        //logger.debug(metadataRegex.split(md).toSeq.toString)
        ((markdownMetadataRegex.split(md) ++ Array("","")).apply(1), metamap)
      case None => (md, Map())
    }
  }
}
