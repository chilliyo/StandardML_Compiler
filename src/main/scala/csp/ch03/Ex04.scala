package csp.ch03

// From SBT: ~run-main csp.ch03.Ex04

object Ex04 {
  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (" ".rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val parens : Parser[Expr] = P (integer | ("(" ~ addSub ~ ")"))
    val mult : Parser[Expr] = P (parens ~ ("*".! ~ parens).rep.map (s => s.toList)).map (foldAssocLeft)
    val addSub : Parser[Expr] = P (mult ~ (("+" | "-").! ~ mult).rep.map (s => s.toList)).map (foldAssocLeft)
  }

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr

  def lookup (env : List[(String, Int)], x : String) : Int = {
    env match {
      case Nil         => throw new RuntimeException (x + " not found")
      case (y, v) :: r => if (x == y) v else lookup (r, x)
    }
  }

  def eval (e : Expr, env : List[(String, Int)]) : Int = {
    e match {
      case CstI (i)           => i
      case Var (x)            => lookup (env, x)
      case Prim ("+", e1, e2) => eval (e1, env) + eval (e2, env)
      case Prim ("*", e1, e2) => eval (e1, env) * eval (e2, env)
      case Prim ("-", e1, e2) => eval (e1, env) - eval (e2, env)
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }

  def pp (e : Expr) : String = {
    e match {
      case CstI (i)          => i.toString
      case Var (x)           => x
      case Prim (op, e1, e2) => "(%s %s %s)".format (pp (e1), op, pp (e2))
    }
  }

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  def testEval (p : Parser[Expr], s : String) : Unit = {
    val result : fastparse.core.Parsed[Expr, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
        println ("Pretty print: %s.".format (pp (value)))
        println ("Evaluates to: %d.".format (eval (value, Nil)))
        
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Expr] = MyParsers.addSub
    testEval (p01, "12")
    testEval (p01, "12+345")
    testEval (p01, "12*345+5")
    testEval (p01, "12+345*5")
    testEval (p01, "(12+345)*5")
    testEval (p01, "(((12+345)))*5")
    testEval (p01, "(((12+345)))* 5")   // fine now!!
    testEval (p01, "(((12+345)) )*5")
    testEval (p01, "(((1 2+345)) )*5")  // does not work (fails in s=>s.toInt if 'digits' declared in MyParsers rather than MyParsersNoWhitespace)
    testEval (p01, "(    (  ( 12 + 345 ) ) ) * 5")
    testEval (p01, " (    (  ( 12 + 345 ) ) ) * 5")  // does not work
    println ("=" * 80)
  }
}