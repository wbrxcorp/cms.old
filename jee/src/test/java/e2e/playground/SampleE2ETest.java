package e2e.playground;

import org.junit.Test;

public class SampleE2ETest extends e2e.E2ETestBase {
  @Test public void testBar()
  {
    driver.get(root() + "/markdown/");  // http://localhost:54587/markdown/ にブラウザでアクセスする
  }
}
