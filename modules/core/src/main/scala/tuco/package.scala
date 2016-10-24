import tuco.free.{ connection => FC }
import tuco.free.{ telnetd    => FT }

import tuco.hi.{ connection      => HC }
import tuco.hi.{ basicterminalio => HBT }

import scalaz._, Scalaz._

package object tuco {

  // Type Aliases
  type SafeShell        = shell.SafeShell
  type Session[A]       = shell.Session[A]
  type Command[F[_], A] = shell.Command[F, A]
  type Commands[A]      = shell.Commands[A]
  type Config           = util.Config
  type Color            = util.Color
  type Expect           = util.Expect

  // N.B. type SessionIO is defined in SessionIOFunctions to circumvent SI-7139
  // N.B. type ServerIO  is defined in ServerIOFunctions  to circumvent SI-7139

  // Companion Aliases
  val Session      = shell.Session
  val Command      = shell.Command
  val Commands     = shell.Commands
  val Builtins     = shell.Builtins
  val CommandShell = shell.CommandShell
  val Config       = util.Config
  val Color        = util.Color
  val Expect       = util.Expect

}
