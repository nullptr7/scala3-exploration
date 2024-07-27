import scala.collection.mutable
import scala.util.{ Failure, Success, Try }

case class Pointy(
    x:         Double,
    y:         Double,
    addToGrid: Boolean = false,
  ):
  def this() = this(0.0, 0.0)

object Something:
  abstract class Animal(val yourAre: String)

  case class Tiger(iAm: String) extends Animal(iAm)

  case class Lion(iAm: String) extends Animal(iAm)

  case class Rabbit(iAm: String) extends Animal(iAm)

  def animalType(animal: Animal): Unit =
    animal match
      case Rabbit(iAm) => println(s"$iAm is Herbivore")
      case x @ _       => println(s"${x.yourAre} is Carnivore")

  object Example:
    private case class User(id: Int, name: String, email: String)

    private val fetchedFromDatabase: mutable.Map[Int, User] = mutable.Map(
      1 -> User(1, "Foo", "foo@in.com")
    )

    private def userIdValidation(userId: Int): Either[Throwable, Unit] =
      Either.cond(userId > 0, (), new RuntimeException("user format error"))

    private def verifyUserIdAvailable(userId: Int): Either[Throwable, Unit] =
      Either.cond(fetchedFromDatabase.contains(userId), (), new RuntimeException("user present"))

    private def insertUser(userId: Int): Either[Throwable, Unit] =
      Right(
        fetchedFromDatabase.addOne((userId, User(userId, "Bar", "bar@in.com")))
      )

    def insertUserById(userId: Int): Either[Throwable, Unit] =
      for {
        _ <- userIdValidation(userId)
        _ <- verifyUserIdAvailable(userId)
        _ <- insertUser(userId)
      } yield ()

    // End of the world
    def main(args: Array[String]): Unit = {
      val userId = 2

      val user = insertUserById(userId)

      user match {
        case Right(_)    => println("User Inserted")
        case Left(error) => println(s"Issue in inserting the user ${error.getMessage}")
      }
    }
