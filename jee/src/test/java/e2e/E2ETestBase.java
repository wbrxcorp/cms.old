package e2e;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;
import org.junit.BeforeClass;
import org.junit.AfterClass;
import org.eclipse.jetty.server.Server;
import org.openqa.selenium.chrome.ChromeDriver;

public class E2ETestBase {
  static {
    modules.config.JavaCompat.loadConfig("standalone");
    String driverPath;
    if (System.getProperty("os.name").startsWith("Windows")) {
      driverPath = "bin/chromedriver.exe";
    } else {
      driverPath = "bin/chromedriver";
    }
    if (new java.io.File(driverPath).isFile()) {
      System.setProperty("webdriver.chrome.driver", driverPath);
    }
    driver = new ChromeDriver();
  }

  static Server server;
  static int port;
  protected static ChromeDriver driver;

  @BeforeClass public static void beforeClass()
  {
    scala.Tuple2<org.eclipse.jetty.server.Server,Integer> tpl = modules.jetty.JavaCompat.startServer();
    server = tpl._1;
    port = tpl._2;
  }

  @AfterClass public static void afterClass() throws Exception
  {
    server.stop();
  }

  protected String root() { return "http://localhost:" + port + "/"; }

  @Before public void before() { driver.get(root()); }
  @After public void after() { driver.close(); }
}
