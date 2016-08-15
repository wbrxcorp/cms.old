package modules

package object flyway extends com.typesafe.scalalogging.slf4j.LazyLogging {
  val defaultLocations = "db/migration"

  def migrate(url:String,username:String,password:String,locations:String=defaultLocations):Unit = {
    val flyway = new org.flywaydb.core.Flyway()
    flyway.setLocations(locations)
    flyway.setDataSource(url, username, password)
    flyway.migrate()
  }

  def migrate(dataSource:javax.sql.DataSource, locations:String):Unit = {
    val flyway = new org.flywaydb.core.Flyway()
    flyway.setLocations(locations)
    flyway.setDataSource(dataSource)
    flyway.migrate()
  }

  def migrate(dataSource:javax.sql.DataSource):Unit = migrate(dataSource, defaultLocations)

  def migrateDefaultDatabase():Unit = {
    val config = modules.config.get

    modules.jndi.getDataSourceFromJNDI(config.defaultJNDIDataSourceName) match {
      case Some(dataSource) =>
      logger.info("Initializing default database by JNDI")
      config.defaultDatabaseMigrationLocations match {
        case Some(defaultDatabaseMigrationLocations) =>
          migrate(dataSource, defaultDatabaseMigrationLocations)
        case None => migrate(dataSource)
      }
      case None =>
        logger.info("Migrating default database schema by JDBC URL")
        config.defaultDatabaseMigrationLocations match {
          case Some(defaultDatabaseMigrationLocations) =>
            migrate(config.defaultDatabaseURL, config.defaultDatabaseUser, config.defaultDatabasePassword, defaultDatabaseMigrationLocations)
          case None => migrate(config.defaultDatabaseURL, config.defaultDatabaseUser, config.defaultDatabasePassword)
        }
    }
  }
}
