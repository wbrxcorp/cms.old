package modules

import java.io.{InputStream,OutputStream,Reader,Writer}
import java.nio.charset.Charset
import scala.language.implicitConversions
import org.apache.commons.io.IOUtils
import org.apache.commons.io.LineIterator

// https://commons.apache.org/proper/commons-io/javadocs/api-release/index.html?org/apache/commons/io/package-summary.html
package object ioutils {
  val defaultCharsetName = "UTF-8"
  implicit def string2charset(charsetName:String):Charset = Charset.forName(charsetName)
  val defaultCharset = string2charset(defaultCharsetName)

  def contentEquals(input1:InputStream, input2:InputStream):Boolean = IOUtils.contentEquals(input1, input2)
  def contentEquals(input1:Reader, input2:Reader):Boolean = IOUtils.contentEquals(input1, input2)
  def contentEqualsIgnoreEOL(input1:Reader, input2:Reader):Boolean = IOUtils.contentEqualsIgnoreEOL(input1, input2)
  def copy(input:InputStream, output:OutputStream):Int = IOUtils.copy(input, output)
  def copy(input:InputStream, output:OutputStream, bufferSize:Int):Long = IOUtils.copy(input, output, bufferSize)
  def copy(input:InputStream, output:Writer, inputEncoding:Charset):Unit = IOUtils.copy(input, output, inputEncoding)
  def copy(input:Reader, output:OutputStream, outputEncoding:Charset):Unit = IOUtils.copy(input, output, outputEncoding)
  def copy(input:Reader, output:Writer):Int = IOUtils.copy(input, output)
  // :
  def lineIterator(input:InputStream, encoding:Charset=defaultCharset):LineIterator = IOUtils.lineIterator(input, encoding)
  def lineIterator(input:Reader):LineIterator = IOUtils.lineIterator(input)
  def read(input:InputStream, buffer:Array[Byte]):Int = IOUtils.read(input, buffer)
  def read(input:InputStream, buffer:Array[Byte], offset:Int, length:Int):Int = IOUtils.read(input, buffer, offset, length)
  // :
  def toByteArray(input:InputStream):Array[Byte] = IOUtils.toByteArray(input)
  // :
  def toByteArray(input:Reader, encoding:Charset=defaultCharset):Array[Byte] = IOUtils.toByteArray(input, encoding)
  // :
  def toString(input:InputStream, encoding:Charset=defaultCharset):String = IOUtils.toString(input, encoding)
  def toString(input:Reader):String = IOUtils.toString(input)
  // :
  def write(data:Array[Byte], output:Writer, encoding:Charset=defaultCharset):Unit = IOUtils.write(data, output, encoding)
  def write(data:Array[Byte], output:Writer, encoding:String):Unit = IOUtils.write(data, output, encoding)
  // :
  def write(data:String, output:OutputStream, encoding:String):Unit = IOUtils.write(data, output, encoding)
}
