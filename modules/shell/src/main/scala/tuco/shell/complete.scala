package tuco
package shell

import scalaz._, Scalaz._

/**
 * A completer takes a prefix and returns either the unique suffix, or a list of "whole word"
 * alternatives to show to the user.
 */
case class Completer(run: String => String \/ List[String]) {

  def plus(c: Completer): Completer =
    Completer { s =>
      (run(s), c.run(s)) match {
        case (-\/(s1), -\/(s2))   => \/-(List(s1, s2))
        case (-\/(s1), \/-(Nil))  => -\/(s1)
        case (-\/(s1), \/-(ss))   => \/-(s1 :: ss)
        case (\/-(Nil), a)        => a
        case (\/-(ss), -\/(s2))   => \/-(ss ++ List(s2))
        case (\/-(ss1), \/-(ss2)) => \/-(ss1 ++ ss2)
      }
    }

}

object Completer {

  def empty: Completer =
    Completer(_ => \/-(Nil))

  implicit val MonoidCompleter: Monoid[Completer] =
    Monoid.instance(_ plus _, empty)

}
