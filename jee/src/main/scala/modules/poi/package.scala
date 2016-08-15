package modules

import scala.collection.JavaConverters._

import org.apache.poi.ss.usermodel.WorkbookFactory._
import org.apache.poi.ss.usermodel.{Workbook,Sheet,Row,Cell}

package object poi {
  def loadExcel(filename:String):Workbook = create(new java.io.File(filename))
  def loadExcel(in:java.io.InputStream):Workbook = create(in)
  def loadExcel(file:java.io.File):Workbook = create(file)

  // https://poi.apache.org/apidocs/org/apache/poi/ss/usermodel/Sheet.html
  // https://poi.apache.org/apidocs/org/apache/poi/ss/usermodel/Row.html
  // https://poi.apache.org/apidocs/org/apache/poi/ss/usermodel/Cell.html
  def toSeq(book:Workbook):Seq[Sheet] = book.iterator.asScala.toSeq
  def toSeq(sheet:Sheet):Seq[Row] = sheet.iterator.asScala.toSeq
  def toSeq(row:Row):Seq[Cell] = row.iterator.asScala.toSeq

  def map[T](sheet:Sheet)(f:Row=>T):Iterator[T] = sheet.iterator.asScala.map(f)
  def map[T](row:Row)(f:Cell=>T):Iterator[T] = row.iterator.asScala.map(f)
}
