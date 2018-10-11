package tuco
package util

import java.util.{ Map => JMap, HashMap => JHashMap, Properties }
import java.util.function.Supplier

import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.shell.Shell

import scala.collection.JavaConverters._
import cats.~>
import cats.data.Kleisli
import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext

import tuco.free.{ telnetd => FT, KleisliInterpreter }
import tuco.free.telnetd.{ TelnetDOp, TelnetDIO }
import tuco.free.connection.ConnectionIO

class Config[F[_]: Sync: LiftIO](
  val shell:                ConnectionIO[Unit],
  val port:                 Int,
  val floodProtection:      Int,
  val maxCon:               Int,
  val timeToWarning:        Int,
  val timeToTimedout:       Int,
  val housekeepingInterval: Int,
  val interpreter:          KleisliInterpreter[IO]
) {

  def start: F[F[Unit]] =
    for {
      t <- newTelnetD
      _ <- FT.start.foldMap(interpreter.TelnetDInterpreter).run(t).to[F]
    } yield FT.stop.foldMap(interpreter.TelnetDInterpreter).run(t).to[F]

  def run[A](ma: TelnetDIO[A]): F[A] =
    newTelnetD.flatMap(a => ma.foldMap(interpreter.TelnetDInterpreter).run(a).to[F])

  private def unsafeToJavaConfig: JavaConfig = {
    val ps = unsafeToProperties
    new JavaConfig {
      def factories() = Config.this.factories
      def properties(): Properties = ps
    }
  }

  private val newTelnetD: F[TelnetD] =
    Sync[F].delay(TelnetD.createTelnetD(unsafeToJavaConfig))

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
      def get() = new SafeShell(shell, interpreter.ConnectionInterpreter) { }
    })
    m
  }

}

object Config {

  private implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  def apply[F[_]: Async](
    shell:                ConnectionIO[Unit],
    port:                 Int,
    floodProtection:      Int = 5,
    maxCon:               Int = 25,
    timeToWarning:        Int = 3600000,
    timeToTimedout:       Int = 60000,
    housekeepingInterval: Int = 1000
  ): Config[F] =
    new Config[F](
      shell,
      port,
      floodProtection,
      maxCon,
      timeToWarning,
      timeToTimedout,
      housekeepingInterval,
      KleisliInterpreter[IO]
    )

}