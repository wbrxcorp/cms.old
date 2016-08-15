package modules

import javax.sql.DataSource

package object jndi {
  def getDataSourceFromJNDI(name:String):Option[DataSource] = {
    (scala.util.Try {
      val initialContext = new javax.naming.InitialContext
      (if (name.startsWith("java:/")) {
        initialContext.lookup(name)
      } else {
        initialContext.lookup("java:/comp/env").asInstanceOf[javax.naming.Context].lookup(name)
      }).asInstanceOf[DataSource]
    }).toOption
  }
}
