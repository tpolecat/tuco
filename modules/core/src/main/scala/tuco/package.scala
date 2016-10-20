import tuco.free.{ connection => FC }
import tuco.free.{ telnetd    => FT }

import tuco.hi.{ connection      => HC }
import tuco.hi.{ basicterminalio => HBT }

import scalaz._, Scalaz._

package object tuco {

  // Type Aliases
  type SessionIO[A]     = FC.ConnectionIO[A]
  type ServerIO[A]      = FT.TelnetDIO[A]
  type SafeShell        = shell.SafeShell
  type Session[A]       = shell.Session[A]
  type Command[F[_], A] = shell.Command[F, A]
  type Commands[A]      = shell.Commands[A]
  type Config           = util.Config

  // Companion Aliases
  val Session      = shell.Session
  val Command      = shell.Command
  val Commands     = shell.Commands
  val Builtins     = shell.Builtins
  val CommandShell = shell.CommandShell
  val Config       = util.Config

}
