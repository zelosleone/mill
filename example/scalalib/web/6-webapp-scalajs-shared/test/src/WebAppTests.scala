package webapp

import utest._

object WebAppTests extends TestSuite {
  def withServer[T](example: cask.main.Main)(f: String => T): T = {
    // Get the free port from the environment variable or use a default
    val port = sys.env.get(mill.constants.EnvVars.MILL_TEST_FREE_PORT)
      .map(_.split(",").head.toInt)
      .getOrElse(8185)

    val server = io.undertow.Undertow.builder
      .addHttpListener(port, "localhost")
      .setHandler(example.defaultHandler)
      .build
    server.start()
    val res =
      try f(s"http://localhost:$port")
      finally server.stop()
    res
  }

  val tests = Tests {
    test("simpleRequest") - withServer(WebApp) { host =>
      val page = requests.get(host).text()
      assert(page.contains("What needs to be done?"))
    }
  }
}
