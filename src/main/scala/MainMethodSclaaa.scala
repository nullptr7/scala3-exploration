object MainMethodSclaaa {
  def main(args: Array[String]): Unit =
    println("Hello")

}

abstract class Animal(val yourAre: String)
case class Tiger(iAm: String) extends Animal(iAm)
case class Lion(iAm: String) extends Animal(iAm)
case class Rabbit(iAm: String) extends Animal(iAm)

def animalType(animal: Animal): Unit =
  animal match
    case Rabbit(iAm) => println(s"$iAm is Herbivore")
    case x @ _       => println(s"${x.yourAre} is Carnivore")
