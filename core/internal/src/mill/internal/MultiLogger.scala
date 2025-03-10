package mill.internal

import fansi.Attrs
import mill.api.{Logger, SystemStreams}

import java.io.{InputStream, PrintStream}

private[mill] class MultiLogger(
    val colored: Boolean,
    val logger1: Logger,
    val logger2: Logger,
    val inStream0: InputStream
) extends Logger {
  override def toString: String = s"MultiLogger($logger1, $logger2)"
  lazy val streams = new SystemStreams(
    new MultiStream(logger1.streams.out, logger2.streams.out),
    new MultiStream(logger1.streams.err, logger2.streams.err),
    inStream0
  )

  private[mill] override lazy val unprefixedStreams: SystemStreams = new SystemStreams(
    new MultiStream(logger1.unprefixedStreams.out, logger2.unprefixedStreams.out),
    new MultiStream(logger1.unprefixedStreams.err, logger2.unprefixedStreams.err),
    inStream0
  )

  def info(s: String): Unit = {
    logger1.info(s)
    logger2.info(s)
  }
  def error(s: String): Unit = {
    logger1.error(s)
    logger2.error(s)
  }
  def ticker(s: String): Unit = {
    logger1.ticker(s)
    logger2.ticker(s)
  }

  def prompt: Logger.Prompt = new Logger.Prompt {

    override def setPromptDetail(key: Seq[String], s: String): Unit = {
      logger1.prompt.setPromptDetail(key, s)
      logger2.prompt.setPromptDetail(key, s)
    }

    private[mill] override def setPromptLine(
        key: Seq[String],
        keySuffix: String,
        message: String
    ): Unit = {
      logger1.prompt.setPromptLine(key, keySuffix, message)
      logger2.prompt.setPromptLine(key, keySuffix, message)
    }

    private[mill] override def reportKey(key: Seq[String]): Unit = {
      logger1.prompt.reportKey(key)
      logger2.prompt.reportKey(key)
    }

    private[mill] override def clearPromptStatuses(): Unit = {
      logger1.prompt.clearPromptStatuses()
      logger2.prompt.clearPromptStatuses()
    }

    private[mill] override def removePromptLine(key: Seq[String]): Unit = {
      logger1.prompt.removePromptLine(key)
      logger2.prompt.removePromptLine(key)
    }

    private[mill] override def setPromptHeaderPrefix(s: String): Unit = {
      logger1.prompt.setPromptHeaderPrefix(s)
      logger2.prompt.setPromptHeaderPrefix(s)
    }

    private[mill] override def withPromptPaused[T](t: => T): T = {
      logger1.prompt.withPromptPaused(logger2.prompt.withPromptPaused(t))
    }

    private[mill] override def withPromptUnpaused[T](t: => T): T = {
      logger1.prompt.withPromptUnpaused(logger2.prompt.withPromptUnpaused(t))
    }

    override def enableTicker: Boolean = logger1.prompt.enableTicker || logger2.prompt.enableTicker

    override def debugEnabled: Boolean = logger1.prompt.debugEnabled || logger2.prompt.debugEnabled
  }
  def debug(s: String): Unit = {
    logger1.debug(s)
    logger2.debug(s)
  }

  private[mill] override def subLogger(path: os.Path, key: String, message: String): Logger = {
    new MultiLogger(
      colored,
      logger1.subLogger(path, key, message),
      logger2.subLogger(path, key, message),
      inStream0
    )
  }

  override def infoColor: Attrs = logger1.infoColor ++ logger2.infoColor
  override def errorColor: Attrs = logger1.errorColor ++ logger2.errorColor
  private[mill] override def logPrefixKey = logger1.logPrefixKey ++ logger2.logPrefixKey

  override def withOutStream(outStream: PrintStream): Logger = {
    new MultiLogger(
      colored,
      logger1.withOutStream(outStream),
      logger2.withOutStream(outStream),
      inStream0
    )
  }
}
