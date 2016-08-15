package modules

package object h2 {
  def getMySQLCompatibleH2InMemoryDatabaseURL(dbname:String):String =
    "jdbc:h2:mem:%s;DB_CLOSE_DELAY=-1;MODE=MySQL;INIT=CREATE SCHEMA IF NOT EXISTS \"public\"".format(dbname)
}
