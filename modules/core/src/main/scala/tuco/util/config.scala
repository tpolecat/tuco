package tuco.util

import tuco.free.telnetd._

import net.wimpi.telnetd.TelnetD
import scalaz._, Scalaz._, scalaz.effect._

final class Config(val props: Map[String, String]) {

  val newTelnetD: IO[TelnetD] =
    IO {
      val ps = new java.util.Properties
      props.foreach { case (k, v) => ps.setProperty(k, v) }
      TelnetD.createTelnetD(ps)
    }

  def run[A](ma: TelnetDIO[A]): IO[A] =
    newTelnetD.flatMap(ma.transK[IO].run)

  def orElse(other: Config): Config =
    new Config(props ++ other.props)

}

object Config extends DefaultConfig {

  val empty: Config =
    new Config(Map.empty)

  implicit val ConfigMonoid: Monoid[Config] =
    new Monoid[Config] {
      val zero = empty
      def append(a: Config, b: => Config) = a orElse b
    }

}

trait DefaultConfig {
  import reflect.ClassTag
  import net.wimpi.telnetd.shell.Shell

  def apply[A <: Shell](port: Int)(implicit ev: ClassTag[A]): Config =
    new Config(Map(

      // List of terminals available and defined below
      "terminals"                -> "vt100,ansi,windoof,xterm",

      // vt100 implementation and aliases
      "term.vt100.class"         -> "net.wimpi.telnetd.io.terminal.vt100",
      "term.vt100.aliases"       -> "default,vt100-am,vt102,dec-vt100",

      // ansi implementation and aliases
      "term.ansi.class"          -> "net.wimpi.telnetd.io.terminal.ansi",
      "term.ansi.aliases"        -> "color-xterm,xterm-color,vt320,vt220,linux,screen",

      // windoof implementation and aliases
      "term.windoof.class"       -> "net.wimpi.telnetd.io.terminal.Windoof",
      "term.windoof.aliases"     -> "",

      // xterm implementation and aliases
      "term.xterm.class"         -> "net.wimpi.telnetd.io.terminal.xterm",
      "term.xterm.aliases"       -> "",

      // List of shells available and defined below
      "shells"                   -> "simple",

      // shell implementations
      "shell.simple.class"       -> ev.runtimeClass.getName,

      // Listeners Section
      "listeners"                -> "std",

      // Basic listener and connection management settings
      "std.port"                 -> port.toString,
      "std.floodprotection"      -> "5",
      "std.maxcon"               -> "25",
      "std.time_to_warning"      -> "3600000",
      "std.time_to_timedout"     -> "60000",
      "std.housekeepinginterval" -> "1000",
      "std.inputmode"            -> "character",
      "std.loginshell"           -> "simple",
      "std.connectionfilter"     -> "none"

    ))


}
