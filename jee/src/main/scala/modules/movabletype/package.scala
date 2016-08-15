package modules

import _root_.scalikejdbc.DB

package object movabletype extends _root_.scalikejdbc.SQLInterpolation {
  def getMovableTypeAuthor(name:String):Option[Author] = {
    DB readOnly { implicit session =>
      sql"select author_id,author_name,author_nickname from mt_author where author_name=${name}".map { row =>
        Author(id=row.int(1),name=row.string(2),nickname=row.stringOpt(3))
      }.single.apply
    }
  }
  def getMovableTypeAuthor(name:String, password:String):Author = {
    // TODO: crypt以外の2タイプの暗号化方式にも対応すること MT/Auth/MT.pm is_valid_password()
    DB readOnly { implicit session =>
      sql"select author_id,author_name,author_nickname,author_password from mt_author where author_name=${name}".map { row =>
        val (id, name, nickname, encryptedPassword) = (row.int(1),row.string(2),row.stringOpt(3),row.string(4))
        if (org.apache.commons.codec.digest.Crypt.crypt(password,encryptedPassword.substring(0,2)) == encryptedPassword) {
          Some(Author(id, name, nickname))
        } else None
      }.single.apply.flatten.getOrElse(throw new SecurityException("Authentication Failed: %s".format(name)))
    }
  }
}
