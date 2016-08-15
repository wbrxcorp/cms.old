package modules

import collection.JavaConversions._

package object velocity {
  def evaluateVelocityTemplate(template:String,variables:Map[String,AnyRef],logTag:String="evaluate"):String = {
    val result = new java.io.StringWriter();
    val javaMap = collection.mutable.Map(variables.toSeq: _*)
    org.apache.velocity.app.Velocity.evaluate(new org.apache.velocity.VelocityContext(javaMap), result, logTag, template)
    result.toString
  }
}
