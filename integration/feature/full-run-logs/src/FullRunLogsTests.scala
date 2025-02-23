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
      val initial = eval(("--ticker", "true", "run", "--text", "hello"))
      initial.isSuccess ==> true

      // Add some code that will generate warnings but not errors
      modifyFile(workspacePath / "src" / "Main.java", content => 
        content.replace("public class", "@Deprecated public class"))

      // Run and verify warnings don't count as failures
      val warned = eval(("--ticker", "true", "run", "--text", "hello"))
      warned.isSuccess ==> true
      // Progress should show no failures despite warnings
      assert(warned.err.contains("[warn]"))
      assert(!warned.err.contains("failed]"))

      // Now break the Java source file with invalid syntax
      modifyFile(workspacePath / "src" / "Main.java", _.replace("public class", "public class class"))

      // Run again and verify the failure is counted
      val failed = eval(("--ticker", "true", "run", "--text", "hello"))
      failed.isSuccess ==> false

      // The error output should show the failure counter in the progress
      val expectedErrorRegex = java.util.regex.Pattern
        .quote(
          s"""<dashes> run --text hello <dashes>
             |[build.mill-<digits>/<digits>] compile
             |[build.mill-<digits>] [info] compiling 1 Scala source to ${tester.workspacePath}/out/mill-build/compile.dest/classes ...
             |[build.mill-<digits>] [info] done compiling
             |[<digits>/<digits>] compile
             |[<digits>] [error] Main.java:1: error: 'class' not expected here
             |[<digits>/<digits>] <dashes> run --text hello <dashes> <digits>s [<digits>/<digits>, 1 failed]"""
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
      val initial = eval(("--ticker", "false", "run", "--text", "hello"))
      initial.isSuccess ==> true

      // Add some code that will generate warnings but not errors
      modifyFile(workspacePath / "src" / "Main.java", content => 
        content.replace("public class", "@Deprecated public class"))

      // Run and verify warnings don't count as failures
      val warned = eval(("--ticker", "false", "run", "--text", "hello"))
      warned.isSuccess ==> true
      // Progress should show no failures despite warnings
      assert(warned.err.contains("[warn]"))
      assert(!warned.err.contains("failed]"))

      // Now break the Java source file with invalid syntax
      modifyFile(workspacePath / "src" / "Main.java", _.replace("public class", "public class class"))

      // Run again and verify the failure is counted
      val failed = eval(("--ticker", "false", "run", "--text", "hello"))
      failed.isSuccess ==> false

      // The error output should show the failure counter without ticker formatting
      val expectedErrorRegex = java.util.regex.Pattern
        .quote(
          s"""[build.mill] [info] compiling 1 Scala source to ${tester.workspacePath}/out/mill-build/compile.dest/classes ...
             |[build.mill] [info] done compiling
             |[error] Main.java:1: error: 'class' not expected here"""
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
