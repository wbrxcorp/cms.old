package modules

package object flyway {
  def migrate(url:String,username:String,password:String,locations:String="db/migration"):Unit = {
    val flyway = new org.flywaydb.core.Flyway()
    flyway.setLocations(locations)
    flyway.setDataSource(url, username, password)
    flyway.migrate()
  }

  def migrateDefaultDatabase():Unit = {
    val config = modules.config.get
    config.defaultDatabaseMigrationLocations match {
      case Some(defaultDatabaseMigrationLocations) =>
        migrate(config.defaultDatabaseURL, config.defaultDatabaseUser, config.defaultDatabasePassword, defaultDatabaseMigrationLocations)
      case None => migrate(config.defaultDatabaseURL, config.defaultDatabaseUser, config.defaultDatabasePassword)
    }

  }
}
