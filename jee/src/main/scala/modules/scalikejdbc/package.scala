package modules

package object scalikejdbc extends com.typesafe.scalalogging.slf4j.LazyLogging {
  import _root_.scalikejdbc._
  val config = modules.config.get
  implicit val session = AutoSession

  _root_.scalikejdbc.GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = true,
    singleLineMode = true,
    logLevel = config.scalikeJDBCLogLevel
  )

  def initDefaultDatabase():Unit = {
    modules.jndi.getDataSourceFromJNDI(config.defaultJNDIDataSourceName) match {
      case Some(dataSource) =>
      logger.info("Initializing default database by JNDI")
        ConnectionPool.singleton(new _root_.scalikejdbc.DataSourceConnectionPool(dataSource))
      case None =>
        logger.info("Initializing default database by JDBC URL")
        ConnectionPool.singleton(config.defaultDatabaseURL, config.defaultDatabaseUser, config.defaultDatabasePassword)
    }
  }

  def p(query:_root_.scalikejdbc.SQL[Nothing,_root_.scalikejdbc.NoExtractor])(implicit session:_root_.scalikejdbc.DBSession):Unit = {
    try {
      val rows = query.toMap.list.apply
      val header = rows.headOption.map { firstRow =>
        firstRow.map(_._1)
      }.getOrElse(Seq("EMPTY RESULT"))

      val rowsArray = rows.map { row =>
        row.map(_._2.toString).toArray
      }.toArray

      println(com.jakewharton.fliptables.FlipTable.of(header.toArray,rowsArray))
    }
    catch {
      case e:java.sql.SQLException => logger.error("SQLException code:%d".format(e.getErrorCode))
    }
  }
}
