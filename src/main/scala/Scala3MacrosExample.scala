object Scala3MacrosExample:
  inline def hello(): Unit = println("Hello, world!")

object Test extends App:
  import Scala3MacrosExample._
  hello()
