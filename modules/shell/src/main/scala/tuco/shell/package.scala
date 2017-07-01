package tuco

import tuco.Tuco.SessionIO

package object shell {

  def runShell[A](init: Session[A]): SessionIO[Session[A]] =
    CommandShell.run(init)

}
