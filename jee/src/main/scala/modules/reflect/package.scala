package modules

import scala.util.Try

package object reflect {
  val runtimeMirror = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)

  def getObjectByFullName(name:String):Option[Any] = {
    Try(runtimeMirror.staticModule(name)).map { module =>
      Some(runtimeMirror.reflectModule(module).instance)
    }.getOrElse(None)
  }
  def getClassByFullName(name:String):Option[Class[_ <: Any]] = {
    Try(Class.forName(name)).map(Some(_)).getOrElse(None)
  }

  def construct(clazz:Class[_ <: Any], param1:String):Any = {
    clazz.getDeclaredConstructor(classOf[String]).newInstance(param1);
  }
}
