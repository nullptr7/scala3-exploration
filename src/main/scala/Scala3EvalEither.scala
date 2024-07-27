import com.github.nullptr7.typeclasses.monad.{ given, _ }
import com.github.nullptr7.typeclasses.numeric.{ given, _ }
import com.github.nullptr7.typeclasses.numeric.Numeric

object Scala3EvalEither extends App {
  enum EvalError:
    case InvalidSymbolName
    case SymbolNotFound
    case DivisionByZero

  type EvalResult[A] = Either[EvalError, A]

//  import TypeLambdasFunctorsErrorHandling.{ *, given }

  given evalResultNumeric[A: Numeric]: Numeric[Either[EvalError, A]] with {
    /*override def add(fa: EvalResult[A], fb: EvalResult[A]): EvalResult[A] =
      fa.fflatMap { a =>
        fb.ffmap { b =>
          // summon[Scala2Numeric.Numeric[A]].add(a, b)
          a + b
        }
      }*/

    // another way
    /*override def add(fa: EvalResult[A], fb: EvalResult[A]): EvalResult[A] =
      fa.fmap2(fb)((a, b) => a + b)*/

//    override def mul(a: Either[EvalError, A], b: Either[EvalError, A]): Either[EvalError, A] = ???

    def isZero(a: EvalResult[A]): Boolean = {
      a match {
        case Right(a) if summon[Numeric[A]].isZero(a) => true
        case _ => false
      }
    }

    def add(fa: EvalResult[A], fb: EvalResult[A]): EvalResult[A] = {
      fa.fmap2(fb)((a, b) => a + b)
    }

    def div(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      if isZero(b) then
        Left(EvalError.DivisionByZero)
      else
        a.fmap2(b)(_ / _)
    }

    def sub(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {

      a.fmap2(b)((a, b) => a - b)
    }

    def mul(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      // Note this could also use map2 but I use flatMap to demonstrate
      // how much extra work you have to do compared to using applicative's
      // map2 method...
      a.flatMap {
        aa =>
          b.map {
            bb =>
              aa * bb
          }
      }
    }

  }

  private enum Exp[A]:
    case Val(value: A) extends Exp[A]
    case Add(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Sub(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Mul(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Div(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Var(identifier: String) extends Exp[A]

  private type Env[A] = Map[String, A]

  import Exp._

  private type WithEnv[A] = Env[A] ?=> Either[EvalError, A]

  private def summonEnv[A]: Env[A] ?=> Env[A] = summon[Env[A]]

  private def eval[A: Numeric](exp: Exp[A]): WithEnv[A] =
    exp match
      case Var(id)    => handleVar(id)
      case Val(value) => Right(value)
      case Add(l, r)  => handleAdd(l, r)
      case Sub(l, r)  => handleSub(l, r)
      case Div(l, r)  => handleDiv(l, r)
      case Mul(l, r)  => handleMul(l, r)

  private def handleAdd[A: Numeric](l: Exp[A], r: Exp[A]): WithEnv[A] = eval(l) + eval(r)

  private def handleSub[A: Numeric](l: Exp[A], r: Exp[A]): WithEnv[A] = eval(l) - eval(r)

  private def handleMul[A: Numeric](l: Exp[A], r: Exp[A]): WithEnv[A] = eval(l) * eval(r)

  private def handleDiv[A: Numeric](l: Exp[A], r: Exp[A]): WithEnv[A] = eval(l) / eval(r)

  private def handleVar[A](s: String): WithEnv[A] =
    summonEnv.get(s) match {
      case Some(value) => Right(value)
      case None        => Left(EvalError.SymbolNotFound)
    }

  /*val exp2: ExpV2[Int] = MulV2(VarV2("1"), VarV2("2"))

  val exp1: ExpV2[Int] = AddV2(
    VarV2("z"),
    AddV2(
      ValV2(10),
      MulV2(
        VarV2("x"),
        VarV2("y"))))
  {
    given envMap: EnvV2[Int] = Map("x" -> 7, "y" -> 6, "a" -> 22)

    val eval1 = evalV2(exp1)

    println(s"Eval exp gives $eval1")
  }
/*
  {
    given envMap: EnvV2[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2)

    val eval1 = evalV2(exp1)

    println(s"Eval exp gives $eval1")
  }*/*/

}
