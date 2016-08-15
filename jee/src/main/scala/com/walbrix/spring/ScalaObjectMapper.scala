package com.walbrix.spring

import scala.collection.JavaConverters._

import com.fasterxml.jackson.databind.{Module, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.springframework.beans.factory.{InitializingBean, BeanClassLoaderAware, FactoryBean}
import org.springframework.context.{ApplicationContext, ApplicationContextAware}

/**
 * Created by shimarin on 15/04/13.
 */

class ScalaObjectMapper extends ObjectMapper with com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper {
  this.registerModule(DefaultScalaModule)
}

class Jackson2ObjectMapperFactoryBean extends FactoryBean[ScalaObjectMapper] with BeanClassLoaderAware with ApplicationContextAware with InitializingBean {

  private var classLoader:ClassLoader = _
  private var applicationContext:ApplicationContext = _
  private var objectMapper:ScalaObjectMapper = _

  private var modules:Seq[Module] = Nil

  override def getObjectType: Class[_] = classOf[ScalaObjectMapper]
  override def getObject: ScalaObjectMapper = this.objectMapper
  override def isSingleton: Boolean = true
  override def setBeanClassLoader(classLoader: ClassLoader): Unit = this.classLoader = classLoader
  override def setApplicationContext(applicationContext: ApplicationContext): Unit = this.applicationContext = applicationContext
  override def afterPropertiesSet(): Unit = {
    if (this.objectMapper != null) {
      this.configure(this.objectMapper)
    }
    else {
      this.objectMapper = this.build
    }
  }


  def configure(objectMapper: ObjectMapper):Unit = {
    modules.foreach(objectMapper.registerModule(_))
  }

  def build:ScalaObjectMapper = {
    val objectMapper = new ScalaObjectMapper
    configure(objectMapper)
    objectMapper
  }


  def setModules(modules:java.util.List[Module]):Unit = this.modules = modules.asScala
}
