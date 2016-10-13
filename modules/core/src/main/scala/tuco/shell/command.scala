package tuco.shell

import scalaz._, Scalaz._

import net.bmjames.opts.Parser

/**
 * Metadata and implementation of an effectful command.
 * @param name The name of the command, as you expect it to be typed by the user. `ls` for example.
 * @param desc A description of the command. `Lists files in the curren directory` for example.
 * @param parser An optparse-applicative `Parser` yielding a effectful state transition.
 */
case class Command[F[_], A](name: String, desc: String, parser: Parser[A => F[A]]) {

  /** Given a lens from `B` to `A` we can zoom out and produce a `Command[F, B]`. */
  def zoom[B](lens: B @> A)(implicit ev: Functor[F]): Command[F, B] =
    copy(parser = parser.map(f => lens =>>= f))

  /** Command is an invariant functor if `F` is a covariant functor. */
  def xmap[B](ab: A => B, ba: B => A)(implicit ev: Functor[F]): Command[F, B] =
    copy(parser = parser.map(f => b => f(ba(b)).map(ab)))

}

object Command {

  // Command is an invariant functor if `F` is a covariant functor.
  implicit def commandInvariant[F[_]: Functor]: InvariantFunctor[Command[F, ?]] =
    new InvariantFunctor[Command[F, ?]] {
      def xmap[A, B](fa: Command[F, A], ab: A => B, ba: B => A): Command[F, B] =
        fa.xmap(ab, ba)
    }

}
