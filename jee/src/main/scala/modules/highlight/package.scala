package modules

package object highlight {
  // サフィックス部分を取り出すための正規表現
  private val suffix = """.+\.(.+)$""".r

  def determineLanguageBySuffix(filename:String):Option[(String, String)] = filename.toLowerCase match {
    case suffix("md") => Some(("Markdown","markdown"))
    case suffix("html") => Some(("HTML", "html"))
    case suffix("css") => Some(("CSS", "css"))
    case suffix("xml") => Some(("XML", "xml"))
    case suffix("pom") => Some(("Maven POM", "xml"))
    case suffix("php") => Some(("PHP", "php"))
    case suffix("jsp") => Some(("JSP", "jsp"))
    case suffix("java") => Some(("Java", "java"))
    case suffix("scala") => Some(("Scala","scala"))
    case suffix("sbt") => Some(("Scala Build Tools","scala"))
    case suffix("js") => Some(("JavaScript", "javascript"))
    case suffix("ts") => Some(("TypeScript", "typescript"))
    case suffix("gradle") => Some(("Gradle Buildfile", "gradle"))
    case suffix("py") => Some(("Python", "python"))
    case suffix("sh") => Some(("Shell Script", "sh"))
    case suffix("sql") => Some(("SQL", "sql"))
    case _ => None
  }
}
