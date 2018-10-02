package tuco
package util

import Tuco._
import scala.sys.process._
import cats.effect._
import cats.implicits._

case class Expect[F[_]: Sync](conf: Config[F], args: List[String] = Nil) {
  def test: F[String] =
    Sync[F].delay {
      val cmd = "expect" :: "-c" :: s"""spawn telnet localhost ${conf.port}""" :: args
      cmd.!!.replace("spawn", "$").replaceAll("\u001b\u0037\u001b\u0038", "").trim
    }

  def run: F[String] = conf.start.flatMap { shutdown =>
    test.attempt.flatMap {
      case Right(s) => shutdown.map(_ => s)
      case Left(e)  => shutdown.flatMap(_ => Sync[F].raiseError(e))
    }
  }

  def expect(arg: String) = copy(args = args ++ List("-c", s"""expect "$arg""""))
  def send(arg: String)   = copy(args = args ++ List("-c", s"""send "$arg""""))
  def sendLn(arg: String) = send(arg + "\\r")

  def dialog(ss: (String, String)*): Expect[F] =
    ss.foldLeft(this) { case (e, (a, b)) => e.expect(a).sendLn(b) }

}
