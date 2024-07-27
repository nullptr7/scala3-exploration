private object Scala2Eval extends App {
  sealed trait Exp

  case class Val(value: Int) extends Exp
  case class Mul(left: Exp, right: Exp) extends Exp
  case class Add(left: Exp, right: Exp) extends Exp
  case class Var(identifier: String) extends Exp

  type Env = Map[String, Int]

  type withEnv = Env ?=> Int

  def eval(exp: Exp)(implicit ev: Env): Int =
    exp match
      case Val(value)       => value
      case Mul(left, right) => handleMul(left, right)
      case Add(left, right) => handleAdd(left, right)
      case Var(identifier)  => handleVar(identifier)

  def evalWithContextual(exp: Exp): withEnv =
    exp match
      case Val(value)       => value
      case Mul(left, right) => handleMul(left, right)
      case Add(left, right) => handleAdd(left, right)
      case Var(identifier)  => handleVar(identifier)

  def handleAdd(left: Exp, right: Exp)(implicit ev: Env): Int = eval(left) + eval(right)
  def handleMul(left: Exp, right: Exp)(implicit ev: Env): Int = eval(left) * eval(right)
  def handleVar(identifier: String)(implicit ev: Env):    Int = ev.getOrElse(identifier, 0)

  def handleAddWithContextualFunction(left: Exp, right: Exp): withEnv = eval(left) + eval(right)
  def handleMulWithContextualFunction(left: Exp, right: Exp): withEnv = eval(left) * eval(right)
  def handleVarWithContextualFunction(identifier: String):    withEnv = {
    val ev: Env = summon[Env]
    ev.getOrElse(identifier, 0)
  }

  def summonEnv: Env ?=> Env = summon[Env]

  def handleVarWithContextualFunctionAnotherWay(identifier: String): withEnv =
    summonEnv.getOrElse(identifier, 0)

  val exp1 = Mul(
    Var("z"),
    Add(
      Val(30),
      Mul(
        Var("x"),
        Var("y"),
      ),
    ),
  )

  implicit val env: Env = Map("x" -> 17, "y" -> 10, "z" -> 23)

  // Need to explicitly give this one because we have given envScala3 below
  val eval1 = eval(exp1)(env)
  val eval2 = eval(exp1)(env)

  println(eval1)

  // Using Scala 3
  enum ExpScala3:
    case Val(value: Int)
    case Mul(left: Exp, right: Exp)
    case Add(left: Exp, right: Exp)
    case Var(identifier: String)

  // given envScala3: Env = Map("x" -> 17, "y" -> 10, "z" -> 23)

  def evalScala3(exp: ExpScala3)(using ev: Env): Int =
    exp match
      case ExpScala3.Val(value)       => value
      case ExpScala3.Mul(left, right) => handleMul(left, right)
      case ExpScala3.Add(left, right) => handleAdd(left, right)
      case ExpScala3.Var(identifier)  => handleVar(identifier)

  def handleAddScala3(left: Exp, right: Exp)(using ev: Env) = eval(left) + eval(right)
  def handleMulScala3(left: Exp, right: Exp)(using ev: Env) = eval(left) * eval(right)
  def handleVarScala3(identifier: String): withEnv = summon[Env].getOrElse(identifier, 0)

  import Scala2Numeric.*
  import Scala2Numeric.ops.*

  sealed trait ExpV2[T]

  case class ValV2[T](value: T) extends ExpV2[T]
  case class MulV2[T](left: ExpV2[T], right: ExpV2[T]) extends ExpV2[T]
  case class AddV2[T](left: ExpV2[T], right: ExpV2[T]) extends ExpV2[T]
  case class VarV2[T](identifier: String) extends ExpV2[T]

  type EnvV2[T] = Map[String, T]

  def handleAddV2[T](
      left:    ExpV2[T],
      right:   ExpV2[T],
    )(using
      ev:      EnvV2[T],
      numeric: Numeric[T],
    ): T = evalV2(left) + evalV2(right)
  def handleMulV2[T](
      left:    ExpV2[T],
      right:   ExpV2[T],
    )(using
      ev:      EnvV2[T],
      numeric: Numeric[T],
    ): T = evalV2(left) * evalV2(right)
  def handleVarV2[T](identifier: String)(implicit ev: EnvV2[T]): T = ev(identifier)

  def evalV2[T](expV2: ExpV2[T])(using env: EnvV2[T], numeric: Numeric[T]): T =
    expV2 match
      case ValV2(value)       => value
      case MulV2(left, right) => handleMulV2(left, right)
      case AddV2(left, right) => handleAddV2(left, right)
      case VarV2(identifier)  => handleVarV2(identifier)

  given EnvV2[String] = Map("x" -> "a", "y" -> "b")

  val exp2: ExpV2[String] = MulV2(VarV2("x"), VarV2("y"))

  val evalV2Answer = evalV2(exp2)

  println(evalV2Answer)

}
