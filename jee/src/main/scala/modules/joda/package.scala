package modules

import org.joda.time.{LocalDateTime,LocalDate,LocalTime}

package object joda {
  def localDateTime:LocalDateTime = new LocalDateTime
  def localDateTime(year:Int,month:Int = 1,day:Int = 1,hour:Int = 0,minute:Int = 0,second:Int = 0,millis:Int = 0):LocalDateTime = {
    new LocalDateTime(year, month, day, hour, minute, second, millis)
  }
  def localDate:LocalDate = new LocalDate
  def localDate(year:Int,month:Int = 1,day:Int = 1):LocalDate = new LocalDate(year, month, day)
  def localTime:LocalTime = new LocalTime
  def localTime(hour:Int,minute:Int=0,second:Int=0,millis:Int=0):LocalTime = new LocalTime(hour,minute,second,millis)
}
