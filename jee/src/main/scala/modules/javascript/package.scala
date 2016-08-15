package modules

package object javascript {
  private val scriptEngineManager = new javax.script.ScriptEngineManager

  def getJavaScriptEngine = scriptEngineManager.getEngineByName("JavaScript")

  def evalJavaScript(script:String):Any = {
    val engine = getJavaScriptEngine
    engine.eval(script)
  }
}
