package modules

import java.awt.{RenderingHints, Color,Graphics2D}
import java.awt.image.BufferedImage
import java.io.InputStream
import javax.imageio.ImageIO

package object image {
  import modules.blobstore._

  private val dummyImageCache = new scala.collection.mutable.HashMap[(Int,Int,String,String), Array[Byte]]
  private val sizespecFormat = """(\d+)[xX](\d+)""".r

  def parseImageDataURI(dataURI:String):(ImageContentType,Array[Byte]) = {
    val (contentType, content) = parseDataURI(dataURI)
    (ImageContentType(contentType), content)
  }

  def toDataURI(contentType:ImageContentType, content:Array[Byte]):String = {
    modules.blobstore.toDataURI(contentType.toString, content)
  }

  def toDataURI(format:ImageFormat, content:Array[Byte]):String = toDataURI(format.toContentType, content)

  def putImage(contentType:ImageContentType, content:Array[Byte]):String = putBlob(contentType.toString, content)
  def getImage(id:String):Option[(ImageContentType,Array[Byte])] = {
    getBlob(id).map { case (contentType, content) =>
      (ImageContentType(contentType), content)
    }
  }

  def dummyImage(width:Int, height:Int, text:String="Now Printing", format:ImageFormat=ImageFormat.PNG):Array[Byte] = {
    dummyImageCache.synchronized {
      dummyImageCache.get((width,height,text,format.toString)).getOrElse {
        val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        val g = bufferedImage.getGraphics.asInstanceOf[Graphics2D]

        // draw here
        val font = g.getFont.deriveFont(50.0f)
        g.setFont(font)
        val bounds = g.getFontMetrics.getStringBounds(text, g)
        g.setStroke(new java.awt.BasicStroke(3))
        g.setColor(new Color(128,128,128))
        g.drawRect(0, 0, width - 1, height - 1)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.setColor(new Color(0,0,0))

        g.drawString(text, ((width-bounds.getWidth)/2).toFloat, (height / 2).toFloat)

        val baos = new java.io.ByteArrayOutputStream()
        javax.imageio.ImageIO.write(bufferedImage, format.toString, baos)
        val img = baos.toByteArray
        dummyImageCache.put((width,height,text,format.toString), img)
        img
      }
    }
  }

  /**
  * maxWidth x maxHeight のバウンディングボックスに収まるように画像をリサイズする
  */
 def resizeImage(source:InputStream, maxWidth:Option[Int], maxHeight:Option[Int]):(Array[Byte], ImageFormat) = {
   // http://stackoverflow.com/questions/21057191/can-i-tell-what-the-file-type-of-a-bufferedimage-originally-was
   val imageInputStream = ImageIO.createImageInputStream(source)
   val readers = ImageIO.getImageReaders(imageInputStream)
    if (!readers.hasNext) throw new IllegalArgumentException("Unsupported image file type")

    val reader = readers.next
    reader.setInput(imageInputStream)
    val original = reader.read(0)
    val formatName = reader.getFormatName

    val (origWidth, origHeight) = (original.getWidth, original.getHeight)
    val (width, height) = (maxWidth, maxHeight) match {
      case (Some(maxWidth), Some(maxHeight)) if maxWidth < origWidth || maxHeight < origHeight =>
       if (origWidth.toDouble / maxWidth > origHeight.toDouble / maxHeight) {
         (maxWidth, origHeight * maxWidth / origWidth)
       } else {
         (origWidth * maxHeight / origHeight, maxHeight)
       }
      case (Some(maxWidth), None) if maxWidth < origWidth =>
       (maxWidth, origHeight * maxWidth / origWidth)
      case (None, Some(maxHeight)) if maxHeight < origHeight =>
       (origWidth * maxHeight / origHeight, maxHeight)
      case _ => (origWidth, origHeight)
    }

    val destination = new BufferedImage(width, height, original.getType)

    val scaled = original.getScaledInstance(width, height, java.awt.Image.SCALE_AREA_AVERAGING)
    destination.getGraphics.drawImage(scaled, 0, 0, width, height, null)

    val baos = new java.io.ByteArrayOutputStream()
    ImageIO.write(destination, formatName, baos)

    (baos.toByteArray, ImageFormat(formatName))
  }

  def resizeImage(source:InputStream, sizespec:Option[String]):(Array[Byte], ImageFormat) = {
    val (maxWidth, maxHeight) = sizespec.map { str =>
      str match {
        case sizespecFormat(width, height) => (Some(width.toInt), Some(height.toInt))
        case _ => throw new IllegalArgumentException("invalid size spec format '%s' (e.g. '320x240')".format(str))
      }
    }.getOrElse((None, None))
    resizeImage(source, maxWidth, maxHeight)
  }
}
