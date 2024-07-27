object CurryingExample:
  def add(a: Int)(b: Int): Int = a + b

  def main(args: Array[String]): Unit =
    val add5 = add(5) _
    println(add5(3))
