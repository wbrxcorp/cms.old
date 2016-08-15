package modules

package object fakephp extends com.typesafe.scalalogging.slf4j.LazyLogging {
  //val regex = """<!--\s*#include\s+virtual=["'](.*)["']\s*-->""".r
  val regex = """(?s)<\?php\s+if\s*\(\s*isset\(\$\_SESSION\[\"(.*?)\"\]\)\s*\)\s*\{\s*\?>(.*?)<\?php\s+\}\s+else\s+\{\s*\?>(.*?)<\?php\s+\}\s*\?>""".r

  def resolvePHP(php:String, sessionVariableResolver:(String=>Option[AnyRef])):String = {
    regex.replaceAllIn(php, { m =>
      val sessionVariableName = m.group(1)
      val replacement = sessionVariableResolver(sessionVariableName) match {
        case Some(x) =>
          logger.debug("session.getAttribute(\"%s\") = %s".format(sessionVariableName, x))
          m.group(2) // session exists
        case None =>
          logger.debug("session \"%s\" does not exist".format(sessionVariableName))
          m.group(3) // session does not exist
      }
      resolvePHP(scala.util.matching.Regex.quoteReplacement(replacement), sessionVariableResolver)
    })
  }
}
