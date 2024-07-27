object Scala2Numeric extends App {

  // Type Class
  trait Numeric[T] {
    def add(a: T, b: T): T
    def mul(a: T, b: T): T

    // it also has some generalize methods, where it uses existing abstract methods
    def square(a: T): T = mul(a, a)

  }
  
  object Numeric:
    // this can be used to summon a numeric(same as implicilty)
    def apply[T](using numeric: Numeric[T]): Numeric[T] = numeric

  implicit val intNumeric: Numeric[Int] = new Numeric[Int] {
    override def add(a: Int, b: Int): Int = a + b

    override def mul(a: Int, b: Int): Int = a * b

  }

  implicit val longNumeric: Numeric[Long] = new Numeric[Long] {
    override def add(a: Long, b: Long): Long = a + b

    override def mul(a: Long, b: Long): Long = a * b

  }

  given Numeric[String] = new Numeric[String] {
    override def add(a: String, b: String): String = a + b

    override def mul(a: String, b: String): String =
      for {
        as <- a
        bs <- b
        s  <- as.toString ++ bs.toString
      } yield s

  }

  object ops:
    implicit class NumericOps[T](a: T)(implicit numeric: Numeric[T]):
      def add(b: T): T = numeric.add(a, b)

      def mul(b: T): T = numeric.mul(a, b)

      def +(b: T): T = add(b)

      def *(b: T): T = mul(b)

  def sumList[T](ts: List[T])(implicit numeric: Numeric[T]): T =
    ts.reduce(numeric.add(_, _))

    import ops.*
    ts.reduce(_ + _)

}
