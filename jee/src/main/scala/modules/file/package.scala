package modules

package object file {
  private val patternLeadingSlashes = """^/*""".r
  private val patternTrailingSlashes = """/*$""".r

  def isdir(path:String):Boolean = new java.io.File(path).isDirectory
  def isfile(path:String):Boolean = new java.io.File(path).isFile
  def joinPath(components:Seq[String]):String = {
    components.foldLeft("") { case (x, y) =>
      (x match {
        case "" => ""
        case x => patternTrailingSlashes.replaceAllIn(x,"") + '/'
      }) + patternTrailingSlashes.replaceAllIn(y, "")
    }
  }
  def joinPath(p1:String,p2:String):String = joinPath(Seq(p1, p2))
  def joinPath(p1:String,p2:String,p3:String):String = joinPath(Seq(p1, p2,p3))
  def mapEachLine[T](path:String)(f:String=>T):Seq[T] = {
    // TBD
    Seq()
  }
}
