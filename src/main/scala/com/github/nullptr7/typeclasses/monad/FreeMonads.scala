package com.github.nullptr7.typeclasses.monad

import scala.collection.mutable

object FreeMonads:
  private trait Monad[M[_]]:
    def pure[A](a: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  private object Monad:
    def apply[M[_]](using monad: Monad[M]): Monad[M] = monad

  private trait ~>[F[_], G[_]]:
    def apply[A](fa: F[A]): G[A]

  private trait Free[M[_], A]:
    import Free.*

    def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)

    def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))

    // format: off
    def foldMap[G[_]: Monad](natTransform: M ~> G): G[A] =
      this match
        case FlatMap(fa, f) => Monad[G].flatMap(fa.foldMap[G](natTransform))(a => f(a)            .foldMap(natTransform))
        //                                     |      G[A]                  |         Free[M, B]  |
        //                                     |      G[A]                  | A =>                  G[B]                |
        case Pure(a)        => Monad[G].pure(a)

        case Suspend(ma)    => natTransform.apply(ma)

  // format: on

  private object Free:
    private def pure[M[_], A](a: A): Free[M, A] = Pure(a)

    def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)

    private case class Pure[M[_], A](a: A) extends Free[M, A]

    private case class FlatMap[M[_], A, B](fa: Free[M, A], f: A => Free[M, B]) extends Free[M, B]

    private case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]

  // uses of free monad
  // sequence computations as data structures
  // attach the monadic type at the end

  // e.g. lets say we are working with a database performing CRUD op

  private trait DBOps[A]
  private case class Create[A](key: String, value: A) extends DBOps[Unit]
  private case class Read[A](key: String)             extends DBOps[A]
  private case class Update[A](key: String, value: A) extends DBOps[A]
  private case class Delete(key: String)              extends DBOps[Unit]

  // definitions
  private type DBMonad[A] = Free[DBOps, A]

  // smart constructors
  private def create[A](key: String, value: A): DBMonad[Unit] =
    Free.liftM[DBOps, Unit](Create(key, value))

  private def read[A](key: String): DBMonad[A] =
    Free.liftM[DBOps, A](Read(key))

  private def update[A](key: String, value: A): DBMonad[A] =
    Free.liftM[DBOps, A](Update(key, value))

  private def delete(key: String): DBMonad[Unit] =
    Free.liftM[DBOps, Unit](Delete(key))

  // Business logic if fixed
  // description of the computation
  private def myLittleProgram: DBMonad[Unit] =
    for {
      _    <- create[String]("123-456", "Foo")
      name <- read[String]("123-456")
      _    <- create[String]("456-789", name.toUpperCase)
      _    <- delete("123-456")
    } yield ()

  // The above is just a program we need an evaluator to interpret the program for e.g. IO

  private case class IO[A](unsafeRun: () => A)

  private object IO:
    def create[A](a: => A): IO[A] = IO(() => a)

  private given ioMonad: Monad[IO] with {
    override def pure[A](a: A): IO[A] = IO(() => a)

    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
      IO(() => f(ma.unsafeRun()).unsafeRun())

  }

  private val myDb: mutable.Map[String, String] = mutable.Map()

  private def serialize[A](a: A): String = a.toString

  private def deserialize[A](value: String): A = value.asInstanceOf[A]

  private val dbOps2IO: DBOps ~> IO = new (DBOps ~> IO) {
    override def apply[A](fa: DBOps[A]): IO[A] = fa match
      case Create(key, value) =>
        IO.create {
          // actual code that uses the database
          println(s"INSERT INTO people(id, name) values ($key, $value)")
          myDb += (key -> serialize(value))
          ()
        }
      case Delete(key)        =>
        IO.create {
          println(s"DELETE FROM people WHERE id = $key")
          myDb.remove(key)
          ()
        }
      case Read(key)          =>
        IO.create {
          println(s"SELECT * FROM person WHERE id = $key limit 1")
          deserialize(myDb(key))
        }
      case Update(key, value) =>
        IO.create {
          println(s"UPDATE people(name=$value) WHERE id = $key")
          val oldVal = myDb(key)
          myDb += (key -> serialize(value))
          deserialize(oldVal)
        }

  }

  private val ioProgram: IO[Unit] = myLittleProgram.foldMap(dbOps2IO)

  def main(args: Array[String]): Unit = ioProgram.unsafeRun()
