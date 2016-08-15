package modules

package object console {
  private def isRunFromSBT = {
    val c = new java.io.CharArrayWriter()
    new Exception().printStackTrace(new java.io.PrintWriter(c))
    c.toString().contains("at sbt.")
  }

  def runConsole:Unit = {
    //http://stackoverflow.com/questions/28052667/scala-repl-crashes-when-started-using-scala-tools-nsc-interpreter
    val repl = new scala.tools.nsc.interpreter.ILoop {
      override def printWelcome() = {
        super.printWelcome()
        processLine(buildinfo.BuildInfo.console_initialCommands)
      }
    }
    val settings = new scala.tools.nsc.Settings
    settings.embeddedDefaults[App]
    settings.usejavacp.value = true
    repl.process(settings)
  }
}
