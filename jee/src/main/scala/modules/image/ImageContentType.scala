package modules.image

class ImageContentType(contentType:String) {
  if (!contentType.startsWith("image/")) throw new IllegalArgumentException("Invalid image content type: %s".format(contentType))
  override def toString:String = contentType.toLowerCase
  def toFormat:ImageFormat = ImageFormat(toString.substring(6).trim)
  def toSuffix:String = toFormat.toSuffix
}

class ImageFormat(format:String) {
  override def toString:String = format.toLowerCase
  def toContentType:ImageContentType = ImageContentType("image/" + toString)
  def toSuffix:String = format match {
    case "jpeg" => "jpg"
    case x => x
  }
}

object ImageContentType {
  def apply(contentType:String):ImageContentType = new ImageContentType(contentType)
}

object ImageFormat {
  def PNG:ImageFormat = apply("png")
  def JPEG:ImageFormat = apply("jpeg")
  def GIF:ImageFormat = apply("gif")

  def apply(format:String):ImageFormat = new ImageFormat(format)
  def fromSuffix(suffix:String):ImageFormat = new ImageFormat(suffix.toLowerCase match {
    case "jpg" => "jpeg"
    case x => x
  })
}
