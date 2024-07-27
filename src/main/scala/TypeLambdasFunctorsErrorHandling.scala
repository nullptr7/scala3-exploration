object TypeLambdasFunctorsErrorHandling extends App {
  trait Functor[F[_]]:
    extension [A, B](fa: F[A]) def ffmap(f: A => B): F[B]

  object Functor:
    def apply[F[_]](using f: Functor[F]) = f

  /*  type StringEither[A] = Either[String, A]
  given Functor[StringEither] with
    extension [A, B](x: StringEither[A])
      def ffmap(f: A => B): StringEither[B] =
        x match
          case Left(err) => Left(err)
          case Right(a)  => Right(f(a))*/

  // We are technically not overriding the extension method but we do have to define below
  /*object SS extends Functor[StringEither]:
    extension [A, B](x: StringEither[A])
      def ffmap(f: A => B): StringEither[B] =
        x match
          case Left(err) => Left(err)
          case Right(a)  => Right(f(a))*/

  // Using Scala 3 given and scala 3 type lambda
  given eitherFunctor[E]: Functor[[A] =>> Either[E, A]] with
    extension [A, B](x: Either[E, A])
      def ffmap(f: A => B): Either[E, B] =
        x match
          case Left(err) => Left(err)
          case Right(a)  => Right(f(a))

  given Functor[List] with
    extension [A, B](x: List[A])
      def ffmap(f: A => B): List[B] =
        x match
          case head :: tail => f(head) :: tail.ffmap(f)
          case Nil          => Nil

  // Using Scala 3 given plus scala 2 type lambda
  /*given eitherFunctor2[E]: Functor[({ type T[A] = Either[E, A] })#T] with
      extension [A, B](x: Either[E, A]) def ffmap(f: A => B): Either[E, B] = ???*/

  // Using Scala 2 type lambda
  /*implicit class EitherFunctor3[E, A](x: Either[E, A]) {
      def ffmap[B](f: A => B): Functor[({ type T[A] = Map[E, A] })#T] = ???

    }*/

  val e1: Either[String, Int] = Right(10)
  val e2: Either[String, Int] = Left("boom")
  val e3 = e1.ffmap(a => a + 1)
  val e4 = e2.ffmap(a => a + 1)

  val l1 = List(10, 12)
  val l2 = l1.ffmap(a => a + 1)

  /*println(e3)
  println(e4)*/


}
