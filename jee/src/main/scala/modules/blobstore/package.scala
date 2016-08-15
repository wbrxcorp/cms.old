package modules

package object blobstore {
  private val data = new scala.collection.mutable.HashMap[String, (String,Array[Byte])]

  def parseDataURI(dataURI:String):(String,Array[Byte]) = {
    val chunks = dataURI.split(";", 2)
    if (chunks.length != 2 || !chunks(0).startsWith("data:") || !chunks(1).startsWith("base64,")) throw new IllegalArgumentException("Invalid Data URI string")
    val contentType = chunks(0).substring(5)
    scala.util.Try( (contentType, org.apache.commons.codec.binary.Base64.decodeBase64(chunks(1).substring(7)) ) )
    .getOrElse(throw new IllegalArgumentException("Invalid base64 stream"))
  }

  def toDataURI(contentType:String, content:Array[Byte]) = {
    "data:%s;base64,%s".format(contentType, org.apache.commons.codec.binary.Base64.encodeBase64String(content))
  }

  def putBlob(contentType:String, content:Array[Byte]):String = {
    val id = modules.hash.sha1(content)
    data.synchronized {
      data.put(id, (contentType, content))
    }
    id
  }

  def getBlob(id:String):Option[(String,Array[Byte])] = {
    data.synchronized(data.get(id.toLowerCase))
  }

  def listBlobs:Unit = {
    val header = Array("id", "contentType", "contentLength")
    val rows = data.synchronized {
      data.map { case (id, (contentType, content)) =>
        Array(id, contentType, "%d".format(content.length))
      }.toArray
    }
    println(com.jakewharton.fliptables.FlipTable.of(header,rows))
  }
}
