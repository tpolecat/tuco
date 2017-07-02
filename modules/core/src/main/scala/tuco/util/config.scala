package tuco
package util

import java.util.{ Map => JMap, HashMap => JHashMap, Properties }
import java.util.function.Supplier

import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.shell.Shell

import scala.collection.JavaConverters._
import cats.effect._

import tuco.free.{ telnetd => FT }
import tuco.free.telnetd.TelnetDIO
import tuco.free.connection.ConnectionIO

case class Config(
  shell: ConnectionIO[Unit],
  port: Int,
  floodProtection: Int = 5,
  maxCon: Int = 25,
  timeToWarning: Int = 3600000,
  timeToTimedout: Int = 60000,
  housekeepingInterval: Int = 1000
) {

  def start: IO[IO[Unit]] =
    for {
      t <- newTelnetD
      _ <- FT.start.transK[IO].run(t)
    } yield FT.stop.transK[IO].run(t)

  def run[A](ma: TelnetDIO[A]): IO[A] =
    newTelnetD.flatMap(ma.transK[IO].run)

  private def unsafeToJavaConfig: JavaConfig = {
    val ps = unsafeToProperties
    new JavaConfig {
      def factories() = Config.this.factories
      def properties(): Properties = ps
    }
  }

  private val newTelnetD: IO[TelnetD] =
    IO(TelnetD.createTelnetD(unsafeToJavaConfig))

  private def unsafeToProperties: Properties = {
    val ps = new Properties
    ps.put("terminals", "vt100,ansi,windoof,xterm")
    ps.put("term.vt100.class", "net.wimpi.telnetd.io.terminal.vt100")
    ps.put("term.vt100.aliases", "default,vt100-am,vt102,dec-vt100")
    ps.put("term.ansi.class", "net.wimpi.telnetd.io.terminal.ansi")
    ps.put("term.ansi.aliases", "color-xterm,xterm-color,vt320,vt220,linux,screen")
    ps.put("term.windoof.class", "net.wimpi.telnetd.io.terminal.Windoof")
    ps.put("term.windoof.aliases", "")
    ps.put("term.xterm.class", "net.wimpi.telnetd.io.terminal.xterm")
    ps.put("term.xterm.aliases", "")
    ps.put("shells", "tuco")
    ps.put("listeners", "std")
    ps.put("std.port", port.toString)
    ps.put("std.floodprotection", floodProtection.toString)
    ps.put("std.maxcon" , maxCon.toString)
    ps.put("std.time_to_warning", timeToWarning.toString)
    ps.put("std.time_to_timedout", timeToTimedout.toString)
    ps.put("std.housekeepinginterval", housekeepingInterval.toString)
    ps.put("std.inputmode", "character")
    ps.put("std.loginshell", "tuco")
    ps.put("std.connectionfilter", "none")
    ps
  }

  private val factories: JMap[String, Supplier[Shell]] = {
    val m = new JHashMap[String, Supplier[Shell]]
    m.put("tuco", new Supplier[Shell] {
      def get() = new SafeShell(shell) { }
    })
    m
  }

}
