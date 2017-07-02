package tuco
package shell

import cats.Monoid

/**
 * A completer takes a prefix and returns either the unique suffix, or a list of "whole word"
 * alternatives to show to the user.
 */
case class Completer(run: String => Either[String, List[String]]) {

  def plus(c: Completer): Completer =
    Completer { s =>
      (run(s), c.run(s)) match {
        case (Left(s1), Left(s2))   => Right(List(s1, s2))
        case (Left(s1), Right(Nil))  => Left(s1)
        case (Left(s1), Right(ss))   => Right(s1 :: ss)
        case (Right(Nil), a)        => a
        case (Right(ss), Left(s2))   => Right(ss ++ List(s2))
        case (Right(ss1), Right(ss2)) => Right(ss1 ++ ss2)
      }
    }

}

object Completer {

  def empty: Completer =
    Completer(_ => Right(Nil))

  implicit val MonoidCompleter: Monoid[Completer] =
    new Monoid[Completer] {
      def empty: Completer = Completer.empty
      def combine(a: Completer, b: Completer) = a plus b
    }

}
