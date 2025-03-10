package mill.api

import java.io.PrintStream

/**
 * The standard logging interface of the Mill build tool.
 *
 * Contains these primary logging methods, in order of increasing importance:
 *
 * - `debug` : internal debug messages normally not shown to the user;
 * mostly useful when debugging issues
 *
 * - `ticker`: short-lived logging output where consecutive lines over-write
 * each other; useful for information which is transient and disposable
 *
 * - `info`: miscellaneous logging output which isn't part of the main output
 * a user is looking for, but useful to provide context on what Mill is doing
 *
 * - `error`: logging output which represents problems the user should care
 * about
 *
 * Also contains the two forwarded stdout and stderr streams, for code executed
 * by Mill to use directly. Typically, these correspond to the stdout and stderr,
 * but when `show` is used both are forwarded to stderr and stdout is only
 * used to display the final `show` output for easy piping.
 */
trait Logger {
  def infoColor: fansi.Attrs = fansi.Attrs.Empty
  def errorColor: fansi.Attrs = fansi.Attrs.Empty
  def colored: Boolean

  private[mill] def unprefixedStreams: SystemStreams = streams
  def streams: SystemStreams

  def info(s: String): Unit
  def debug(s: String): Unit
  def error(s: String): Unit
  def ticker(s: String): Unit

  private[mill] def prompt: Logger.Prompt

  private[mill] def subLogger(path: os.Path, keySuffix: String, message: String): Logger =
    this

  private[mill] def withPromptLine[T](t: => T): T = {
    prompt.setPromptLine(logPrefixKey, keySuffix, message)
    try t
    finally prompt.removePromptLine(logPrefixKey)
  }

  def withOutStream(outStream: PrintStream): Logger = this
  private[mill] def message: String = ""
  private[mill] def keySuffix: String = ""
  private[mill] def logPrefixKey: Seq[String] = Nil
  final def debugEnabled = prompt.debugEnabled
}

object Logger {

  /**
   * APIs that allow a logger to interact with the global prompt: setting and unsetting
   * lines, enabling or disabling the prompt, etc. Normally passed through from logger
   * to logger unchanged without any customization.
   */
  trait Prompt {
    private[mill] def setPromptDetail(key: Seq[String], s: String): Unit
    private[mill] def reportKey(key: Seq[String]): Unit
    private[mill] def setPromptLine(key: Seq[String], keySuffix: String, message: String): Unit
    private[mill] def setPromptHeaderPrefix(s: String): Unit
    private[mill] def clearPromptStatuses(): Unit
    private[mill] def removePromptLine(key: Seq[String]): Unit
    private[mill] def withPromptPaused[T](t: => T): T
    private[mill] def withPromptUnpaused[T](t: => T): T

    def debugEnabled: Boolean

    def enableTicker: Boolean
  }
  object Prompt {
    class NoOp extends Prompt {
      private[mill] def setPromptDetail(key: Seq[String], s: String): Unit = ()
      private[mill] def reportKey(key: Seq[String]): Unit = ()
      private[mill] def setPromptLine(key: Seq[String], keySuffix: String, message: String): Unit =
        ()
      private[mill] def setPromptHeaderPrefix(s: String): Unit = ()
      private[mill] def clearPromptStatuses(): Unit = ()
      private[mill] def removePromptLine(key: Seq[String]): Unit = ()
      private[mill] def withPromptPaused[T](t: => T): T = t
      private[mill] def withPromptUnpaused[T](t: => T): T = t

      def debugEnabled: Boolean = false

      def enableTicker: Boolean = false
    }
  }
}
