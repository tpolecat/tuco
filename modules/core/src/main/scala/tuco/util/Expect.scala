package tuco
package util

import Tuco._
import scala.sys.process._
import scalaz._, Scalaz._, scalaz.effect._

case class Expect(conf: Config, args: List[String] = Nil) {
  def test: IO[String] =
    IO {
      val cmd = "expect" :: "-c" :: s"""spawn telnet localhost ${conf.port}""" :: args
      cmd.!!.replace("spawn", "$").replaceAll("\u001b\u0037\u001b\u0038", "").trim
    }
  def run: IO[String] = conf.start.flatMap(test.ensuring)
  def expect(arg: String) = copy(args = args ++ List("-c", s"""expect "$arg""""))
  def send(arg: String)   = copy(args = args ++ List("-c", s"""send "$arg""""))
  def sendLn(arg: String) = send(arg + "\\r")
}
