package modules

import _root_.scalikejdbc.NamedDB
import modules.common.using

package object cms extends _root_.scalikejdbc.SQLInterpolation {

  def convert(srcPath:String="./cms", dstHost:String = "localhost", dstDatabase:String = "cms", dstUser:String = "cms", dstPassword:String = "", dstTimezone:String = "Asia/Tokyo"):Unit = {
    val srcURL = "jdbc:h2:%s;MVCC=true;;AUTO_SERVER=true;DB_CLOSE_DELAY=-1".format(srcPath)
    _root_.scalikejdbc.ConnectionPool.add("src", srcURL, "sa", "")


    val dstURL = "jdbc:mysql://%s/%s?useUnicode=true&characterEncoding=UTF8&zeroDateTimeBehavior=convertToNull&useCompression=true&autoReconnect=true&socketTimeout=10000&useLegacyDatetimeCode=false&serverTimezone=%s".format(dstHost, dstDatabase, dstTimezone)

    val flyway = new org.flywaydb.core.Flyway()
    flyway.setValidateOnMigrate(false)
    flyway.setDataSource(dstURL, dstUser, dstPassword)
    flyway.setLocations("db/cms/migration")
    flyway.migrate()

    _root_.scalikejdbc.ConnectionPool.add("dst", dstURL, dstUser, dstPassword)

    val sqlUpdates = NamedDB("src") readOnly { implicit srcSession =>
      sql"select * from users".map { row =>
          sql"insert ignore into users(id,username,email,password,nickname,auth_token,auth_token_expires_at,admin_user,created_at,updated_at) values(${row.int("id")},${row.string("username")},${row.stringOpt("email")},${row.string("password")},${row.stringOpt("nickname")},${row.string("auth_token")},${row.string("auth_token_expires_at")},${row.boolean("admin_user")},${row.jodaLocalDateTime("created_at")},${row.jodaLocalDateTime("updated_at")})".update
      }.list.apply ++
      sql"select * from entries".map { row =>
        sql"insert ignore into entries(id,user_id,prefix,name,title,description,page_image,content,data,format,visible,published_at,created_at,updated_at) values(${row.int("id")},${row.int("user_id")},${row.string("prefix")},${row.string("name")},${row.string("title")},${row.stringOpt("description")},${row.stringOpt("page_image")},${row.stringOpt("content")},${row.stringOpt("data")},${row.string("format")},${row.boolean("visible")},${row.jodaLocalDateTime("published_at")},${row.jodaLocalDateTime("created_at")},${row.jodaLocalDateTime("updated_at")})".update
      }.list.apply ++
      sql"select * from entries_labels".map { row =>
        sql"replace into entries_labels(entry_id,label) values(${row.int("entry_id")},${row.string("label")})".update
      }.list.apply ++
      sql"select * from prefixes".map { row =>
        sql"replace into prefixes(prefix,user_id,data) values(${row.string("prefix")},${row.intOpt("user_id")},${row.stringOpt("data")})".update
      }.list.apply ++
      sql"select prefix,name,description,user_id,content_type,content,created_at,updated_at from media".map { row =>
        val (hash, size) = using(row.blob("content").getBinaryStream) { in =>
          val bytes = org.apache.commons.io.IOUtils.toByteArray(in)
          (modules.hash.sha1(bytes), bytes.length)
        }
        sql"replace into media(prefix,hash,content_type,content_length,title,description,user_id,created_at,updated_at) values(${row.string("prefix")},${hash},${row.string("content_type")},${size},${row.string("name")},${row.string("description")},${row.intOpt("user_id")},${row.jodaLocalDateTime("created_at")},${row.jodaLocalDateTime("updated_at")})".update
      }.list.apply ++
      sql"select content from media".map { row =>
        val bytes = using(row.blob("content").getBinaryStream) { in => org.apache.commons.io.IOUtils.toByteArray(in) }
        val hash = modules.hash.sha1(bytes)
        sql"replace into blobs(id,content) values(${hash},${bytes})".update
      }.list.apply
    }

    NamedDB("dst") localTx { implicit dstSression => sqlUpdates.foreach(_.apply)}
  }
}
