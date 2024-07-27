import TypeLambdasFunctorsErrorHandling.{ *, given }
object FunctionalErrorHandlingWithApplicative extends App {
  val l1 = List(1, 2, 3)

  // Functor law #1 identity law
  println(l1 == l1.ffmap(a => identity(a)))

  val either1: Either[String, Int] = Right(42)

  println(either1 == either1.ffmap(a => identity(a)))

  // Composition of morphisms

  def f(a: Int): Int = a + 1
  def g(a: Int): Int = a - 1

  println(l1.ffmap(a => f(g(a))) == l1.ffmap(a => g(f(a))))
  println(l1.ffmap(f).ffmap(g) == l1.ffmap(g).ffmap(f))

  println(either1.ffmap(a => f(g(a))) == either1.ffmap(a => g(f(a))))
  println(either1.ffmap(f).ffmap(g) == either1.ffmap(g).ffmap(f))



}
