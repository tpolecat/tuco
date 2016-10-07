package tuco

object imports {

  type BasicTerminalIOIO[A] = free.basicterminalio.BasicTerminalIOIO[A]
  type ConnectionIO[A]      = free.connection.ConnectionIO[A]

  type SafeShell   = shell.SafeShell
  type Session[A]  = shell.Session[A]
  type Command[A]  = shell.Command[A]
  type Commands[A] = shell.Commands[A]

  val Session      = shell.Session
  val Command      = shell.Command
  val Commands     = shell.Commands
  val Builtins     = shell.Builtins
  val CommandShell = shell.CommandShell

  val FB = free.basicterminalio
  val FC = free.connection

  val HB = hi.basicterminalio
  val HC = hi.connection


}
