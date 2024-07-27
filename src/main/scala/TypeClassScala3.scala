import scala.annotation.targetName

private object TypeClassScala3 extends App {
  private trait Numeric[T]:
    def add(a:    T, b: T): T
    def mul(a:    T, b: T): T
    /*
      extension (a: T) def square: T = mul(a, a)
      extension (a: T) def +(b: T) = add(a, b)
      extension (a: T) def *(b: T) = mul(a, b)
     */
    extension (a: T) {
      def square: T = mul(a, a)
      def +(b: T) = add(a, b)
      def *(b: T) = mul(a, b)

    }

  private given Numeric[Int] with
    override def add(a: Int, b: Int): Int = a + b
    override def mul(a: Int, b: Int): Int = a * b

  private given Numeric[String] with
    override def add(a: String, b: String): String = a + b
    override def mul(a: String, b: String): String = for {
      as <- a
      bs <- b
      s  <- as.toString ++ bs.toString
    } yield s

  private val v1 = summon[Numeric[String]].square("ab")

  println(v1)

  // In scala 2 boiler plate
  private object ops:
    implicit class NumericOps[T](a: T)(implicit numeric: Numeric[T]):
      def add(b: T): T = numeric.add(a, b)

      def mul(b: T): T = numeric.mul(a, b)

      def +(b: T): T = add(b)

      def *(b: T): T = mul(b)

  private val v2 = "abcd" * "efgh"

  println(v2)

  private val v3 = 10.square
  println(v3)

}
