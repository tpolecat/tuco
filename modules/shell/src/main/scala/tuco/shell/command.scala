package tuco.shell

import cats.{ Applicative, Functor }
import cats.Invariant
import cats.implicits._
import com.monovore.decline._
import monocle.Lens

/**
 * Metadata and implementation of an effectful command.
 * @param name The name of the command, as you expect it to be typed by the user. `ls` for example.
 * @param desc A description of the command. `Lists files in the curren directory` for example.
 * @param parser A decline `Opts` yielding a effectful state transition.
 */
case class Command[F[_], A](
  name: String,
  desc: String,
  parser: Opts[A => F[A]],
  complete: (A, String) => F[List[String]]
) { outer =>

  /** Given a lens from `B` to `A` we can zoom out and produce a `Command[F, B]`. */
  def zoom[B](lens: Lens[B, A])(implicit ev: Functor[F]): Command[F, B] =
    copy(
      parser   = parser.map(f => lens.modifyF(f)),
      complete = (b, s) => outer.complete(lens.get(b), s)
    )

  /** Command is an invariant functor if `F` is a covariant functor. */
  def xmap[B](ab: A => B, ba: B => A)(implicit ev: Functor[F]): Command[F, B] =
    copy(
      parser   = parser.map(f => b => f(ba(b)).map(ab)),
      complete = (b, s) => outer.complete(ba(b), s)
    )

}

object Command {

  def apply[F[_]: Applicative, A](name: String, desc: String, parser: Opts[A => F[A]]): Command[F, A] =
    apply(name, desc, parser, (a, s) => List.empty[String].pure[F])

  // Command is an invariant functor if `F` is a covariant functor.
  implicit def commandInvariant[F[_]: Functor]: Invariant[Command[F, ?]] =
    new Invariant[Command[F, ?]] {
      def imap[A, B](fa: Command[F, A])(ab: A => B)(ba: B => A): Command[F, B] =
        fa.xmap(ab, ba)
    }

}
