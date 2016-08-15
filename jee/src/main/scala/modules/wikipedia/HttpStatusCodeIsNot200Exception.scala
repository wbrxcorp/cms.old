package modules.wikipedia

class HttpStatusCodeIsNot200Exception(val code:Int) extends RuntimeException {}
