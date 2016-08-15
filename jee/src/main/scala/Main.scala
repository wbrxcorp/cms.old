import sun.misc.Signal

object Main extends App {
  // 第1引数をプロファイル名として使用、省略時は "standalone" (modules.config.DefaultStandaloneConfig 又は profiles.standalone.Config)
  modules.config.loadConfig(args.toSeq.headOption.getOrElse("standalone"))

  val (server, port) = modules.jetty.startServer
  println("http://localhost:%d".format(port))
  println("Ctrl+C to stop running")
  scala.util.Try(Signal.handle(new Signal("INT"), new sun.misc.SignalHandler {
    def handle(sig:Signal):Unit = server.stop
  }))
}

object ConsoleMain extends App {
  modules.config.loadConfig(args.toSeq.headOption.getOrElse("standalone"))
  modules.console.runConsole
}
