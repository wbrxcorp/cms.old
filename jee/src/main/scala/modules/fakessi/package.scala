package modules

package object fakessi {
  val regex = """<!--\s*#include\s+virtual=["'](.*)["']\s*-->""".r

  def resolveShtml(shtml:String, resourceResolver:String=>String):String = {
    regex.replaceAllIn(shtml, { m =>
      resolveShtml(scala.util.matching.Regex.quoteReplacement(resourceResolver(m.group(1))), resourceResolver)
    })
  }
}
