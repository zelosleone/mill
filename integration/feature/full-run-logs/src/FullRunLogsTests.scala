package mill.integration

import mill.constants.OutFiles
import mill.testkit.UtestIntegrationTestSuite
import utest.*

// Run simple commands on a simple build and check their entire output and some
// metadata files, ensuring we don't get spurious warnings or logging messages
// slipping in and the important parts of the logs and output files are present
object FullRunLogsTests extends UtestIntegrationTestSuite {

  def tests: Tests = Tests {
    test("noticker") - integrationTest { tester =>
      import tester._

      val res = eval(("--ticker", "false", "run", "--text", "hello"))
      res.isSuccess ==> true
      assert(res.out == "<h1>hello</h1>")
      assert(
        res.err.replace('\\', '/').replaceAll("(\r\n)|\r", "\n") ==
          s"""[build.mill] [info] compiling 1 Scala source to ${tester.workspacePath}/out/mill-build/compile.dest/classes ...
             |[build.mill] [info] done compiling
             |[info] compiling 1 Java source to ${tester.workspacePath}/out/compile.dest/classes ...
             |[info] done compiling""".stripMargin.replace('\\', '/').replaceAll("(\r\n)|\r", "\n")
      )
    }
    test("ticker") - integrationTest { tester =>
      import tester._

      val res = eval(("--ticker", "true", "run", "--text", "hello"))
      res.isSuccess ==> true
      assert("\\[\\d+\\] <h1>hello</h1>".r.matches(res.out))

      val expectedErrorRegex = java.util.regex.Pattern
        .quote(
          s"""<dashes> run --text hello <dashes>
             |[build.mill-<digits>/<digits>] compile
             |[build.mill-<digits>] [info] compiling 1 Scala source to ${tester.workspacePath}/out/mill-build/compile.dest/classes ...
             |[build.mill-<digits>] [info] done compiling
             |[<digits>/<digits>] compile
             |[<digits>] [info] compiling 1 Java source to ${tester.workspacePath}/out/compile.dest/classes ...
             |[<digits>] [info] done compiling
             |[<digits>/<digits>] run
             |[<digits>/<digits>] <dashes> run --text hello <dashes> <digits>s [<digits>/<digits>]"""
            .stripMargin
            .replaceAll("(\r\n)|\r", "\n")
            .replace('\\', '/')
        )
        .replace("<digits>", "\\E\\d+\\Q")
        .replace("<dashes>", "\\E=+\\Q")

      val normErr = res.err.replace('\\', '/').replaceAll("(\r\n)|\r", "\n")
      assert(expectedErrorRegex.r.matches(normErr))
    }
    test("show") - integrationTest { tester =>
      import tester._
      // Make sure when we have nested evaluations, e.g. due to usage of evaluator commands
      // like `show`, both outer and inner evaluations hae their metadata end up in the
      // same profile files so a user can see what's going on in either
      eval(("show", "compile"))
      val millProfile = ujson.read(os.read(workspacePath / OutFiles.out / "mill-profile.json")).arr
      val millChromeProfile =
        ujson.read(os.read(workspacePath / OutFiles.out / "mill-chrome-profile.json")).arr
      // Profile logs for the thing called by show
      assert(millProfile.exists(_.obj("label").str == "compile"))
      assert(millProfile.exists(_.obj("label").str == "compileClasspath"))
      assert(millProfile.exists(_.obj("label").str == "ivyDeps"))
      assert(millProfile.exists(_.obj("label").str == "javacOptions"))
      assert(millChromeProfile.exists(_.obj("name").str == "compile"))
      assert(millChromeProfile.exists(_.obj("name").str == "compileClasspath"))
      assert(millChromeProfile.exists(_.obj("name").str == "ivyDeps"))
      assert(millChromeProfile.exists(_.obj("name").str == "javacOptions"))
      // Profile logs for show itself
      assert(millProfile.exists(_.obj("label").str == "show"))
      assert(millChromeProfile.exists(_.obj("name").str == "show"))
    }
    test("failureCounter.interactive") - integrationTest { tester =>
      import tester._

      // First verify normal successful compilation
      val initial = eval(("--ticker", "true", "compile", "foo", "bar"))
      initial.isSuccess ==> true

      // Add some deprecated method usage
      modifyFile(workspacePath / "build.mill", content => 
        content + "\nobject foo extends Module { def foo = T { resolveDeps(Agg()) } }")

      // Run and verify deprecation warnings don't count as failures
      val warned = eval(("--ticker", "true", "foo.foo"))
      warned.isSuccess ==> true
      // Should contain warning but no failure count
      assert(warned.err.contains("[warn]"))
      assert(warned.err.contains("is deprecated"))
      assert(!warned.err.contains("failed]"))

      // Now add actual failures
      modifyFile(workspacePath / "build.mill", content => 
        content + "\nobject bar extends Module { def bar = T { throw new Exception(\"bar failed\") } }")

      // Run and verify only real failures are counted
      val failed = eval(("--ticker", "true", "foo.foo", "bar.bar"))
      failed.isSuccess ==> false

      // The error output should show only actual failures in the counter, not warnings
      val expectedErrorRegex = java.util.regex.Pattern
        .quote(
          s"""<dashes> foo.foo bar.bar <dashes>
             |[<digits>/<digits>] foo.foo
             |[<digits>] [warn] method resolveDeps is deprecated
             |[<digits>/<digits>] bar.bar
             |[<digits>] [error] bar failed
             |[<digits>/<digits>] <dashes> foo.foo bar.bar <dashes> <digits>s [<digits>/<digits>, 1 failed]"""
            .stripMargin
            .replaceAll("(\r\n)|\r", "\n")
            .replace('\\', '/')
        )
        .replace("<digits>", "\\E\\d+\\Q")
        .replace("<dashes>", "\\E=+\\Q")

      val normErr = failed.err.replace('\\', '/').replaceAll("(\r\n)|\r", "\n")
      assert(expectedErrorRegex.r.matches(normErr))
    }

    test("failureCounter.ci") - integrationTest { tester =>
      import tester._

      // First verify normal successful compilation
      val initial = eval(("--ticker", "false", "compile", "foo", "bar"))
      initial.isSuccess ==> true

      // Add some deprecated method usage
      modifyFile(workspacePath / "build.mill", content => 
        content + "\nobject foo extends Module { def foo = T { resolveDeps(Agg()) } }")

      // Run and verify deprecation warnings don't count as failures
      val warned = eval(("--ticker", "false", "foo.foo"))
      warned.isSuccess ==> true
      // Should contain warning but no failure count
      assert(warned.err.contains("[warn]"))
      assert(warned.err.contains("is deprecated"))
      assert(!warned.err.contains("failed"))

      // Now add actual failures
      modifyFile(workspacePath / "build.mill", content => 
        content + "\nobject bar extends Module { def bar = T { throw new Exception(\"bar failed\") } }")

      // Run and verify only real failures are counted
      val failed = eval(("--ticker", "false", "foo.foo", "bar.bar"))
      failed.isSuccess ==> false

      // The error output should show only actual failures, not warnings
      val expectedErrorRegex = java.util.regex.Pattern
        .quote(
          s"""[warn] method resolveDeps is deprecated
             |[error] bar failed
             |1 task failed"""
            .stripMargin
            .replaceAll("(\r\n)|\r", "\n")
            .replace('\\', '/')
        )
        .replace("<digits>", "\\E\\d+\\Q")
        .replace("<dashes>", "\\E=+\\Q")

      val normErr = failed.err.replace('\\', '/').replaceAll("(\r\n)|\r", "\n")
      assert(expectedErrorRegex.r.matches(normErr))
    }
  }
}
