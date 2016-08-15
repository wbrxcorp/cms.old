package modules

package object trireme {
  def execjs():Int = {
    // DOESN'T WORK DUE TO OLD VERSION OF RHINO THAT YUI Compressor RELIES ON
    val env = new io.apigee.trireme.core.NodeEnvironment
    val script = env.createScript("my-test-script.js", "console.log('hello, world!')", null);
    val status = script.execute.get
    status.getExitCode
  }
}
