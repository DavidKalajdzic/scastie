package com.olegych.scastie.api

import java.io.{PrintWriter, StringWriter}
import play.api.libs.json._

case class RuntimeError(message: String,
                        line: Option[Int],
                        fileName: Option[String],
                        fullStack: String)

object RuntimeError {
  implicit val formatRuntimeError: OFormat[RuntimeError] = Json.format[RuntimeError]

  def wrap[T](in: => T): Either[Option[RuntimeError], T] = {
    try {
      Right(in)
    } catch {
      case ex: Exception =>
        Left(RuntimeError.fromThrowable(ex, fromScala = false))
    }
  }

  def fromThrowable(t: Throwable, fromScala: Boolean = true): Option[RuntimeError] = {
    def search(e: Throwable) = {
      e.getStackTrace
        .find((trace: StackTraceElement) =>
          if (fromScala) {
            trace.getLineNumber != -1
          } else true
        )
        .map(traceElement => (e, Some(traceElement.getLineNumber), Option(traceElement.getFileName)))
    }

    def loop(e: Throwable): Option[(Throwable, Option[Int], Option[String])] = {
      val s = search(e)
      if (s.isEmpty)
        if (e.getCause != null) loop(e.getCause)
        else Some((e, None, None))
      else s
    }

    loop(t).map {
      case (err, line, fileName) =>
        val errors = new StringWriter()
        t.printStackTrace(new PrintWriter(errors))
        val fullStack = errors.toString

        RuntimeError(err.toString, line, fileName, fullStack)
    }
  }
}

object RuntimeErrorWrap {
  implicit val formatRuntimeErrorWrap: OFormat[RuntimeErrorWrap] = Json.format[RuntimeErrorWrap]
}

case class RuntimeErrorWrap(error: Option[RuntimeError])
