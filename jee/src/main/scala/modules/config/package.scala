package modules

package object config extends com.typesafe.scalalogging.slf4j.LazyLogging {
  val defaultConfigs = Map("default"->DefaultDefaultConfig, "standalone"->DefaultStandaloneConfig)
  var currentConfig:Config = DefaultDefaultConfig // loadConfigを一度も呼び出さないと DefaultDefaultConfigが使用される

  def loadConfig(profile:String="default"):Unit = {
    this.synchronized {
      currentConfig =
        modules.reflect.getObjectByFullName("profiles.%s.Config".format(profile)).map(_.asInstanceOf[Config])
          .getOrElse(defaultConfigs(profile))
      logger.info("currentConfig: %s".format(currentConfig.toString))
    }
  }

  def get:Config = this.synchronized { currentConfig }
}
