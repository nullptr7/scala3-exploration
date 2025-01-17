package com.github.nullptr7.typeclasses.applicative

import com.github.nullptr7.typeclasses.functor.Functor

object Applicative:
  def apply[F[_]](using a: Applicative[F]) = a

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](x: A): F[A]

  extension [A, B](x: F[A])
    def ap(f: F[A => B]): F[B]

    def fmap(f: A => B): F[B] =
      x.ap(pure(f))

  extension [A, B, C](fa: F[A])
    def fmap2(fb: F[B])(f: (A, B) => C): F[C] = {
      val fab: F[B => C] = fa.fmap((a: A) => (b: B) => f(a, b))
      fb.ap(fab)
    }

end Applicative

given eitherApplicative[Err]: Applicative[[X] =>> Either[Err, X]] with {
  def pure[A](a: A): Either[Err, A] = Right(a)

  extension [A, B](x: Either[Err, A])
    def ap(f: Either[Err, A => B]) =
      (f, x) match {
        case (Right(f), Right(x)) => Right(f(x))
        case (Left(err), _)       => Left(err)
        case (_, Left(err))       => Left(err)
      }

}

given optionApplicative: Applicative[Option] with {
  def pure[A](a: A): Option[A] = Option(a)

  extension [A, B](fa: Option[A])
    def ap(ff: Option[A => B]): Option[B] =
      (fa, ff) match {
        case (Some(a), Some(f)) => Some(f(a))
        case _                  => None
      }

}

given listApplicative: Applicative[List] with {
  def pure[A](a: A): List[A] = List(a)

  extension [A, B](as: List[A])
    def ap(fs: List[A => B]): List[B] =
      fs match {
        case f :: tl => as.fmap(f) ++ as.ap(tl)
        case Nil     => Nil
      }

}
