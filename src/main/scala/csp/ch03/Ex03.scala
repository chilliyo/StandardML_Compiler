package csp.ch03

import fastparse.all._

// From SBT: ~run-main csp.ch03.Ex03

object Ex03 {
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

  def test [X] (p : Parser[X], s : String) : Unit = {
    val result : fastparse.core.Parsed[X, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

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

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)

    val p01 : Parser[(Int,Seq[(String,Int)])] = P (digits ~ (("+" | "-").! ~ digits).rep)
    test (p01, "12")
    test (p01, "12+345")
    test (p01, "12+345-5")
    test (p01, "8-3-1")
    println ("=" * 80)

    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val p02 : Parser[(Expr,Seq[(String,Expr)])] = P (integer ~ (("+" | "-").! ~ integer).rep) 
    test (p02, "12")
    test (p02, "12+345")
    test (p02, "12+345-5")
    test (p02, "8-3-1")
    println ("=" * 80)

    val p03 : Parser[(Expr,List[(String,Expr)])] = P (integer ~ (("+" | "-").! ~ integer).rep.map(s => s.toList)) 
    test (p03, "12")
    test (p03, "12+345")
    test (p03, "12+345-5")
    test (p03, "8-3-1")
    println ("=" * 80)

    def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
      p match {
        case (e1, Nil) => e1
        case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
      }
    }

    val p04 : Parser[Expr] = P (integer ~ (("+" | "-").! ~ integer).rep.map (s => s.toList)).map (foldAssocLeft)
    testEval (p04, "12")
    testEval (p04, "12+345")
    testEval (p04, "12+345-5")
    testEval (p04, "8-3-1")
    println ("=" * 80)

    def foldAssocRight (p : (Expr, List[(String,Expr)])) : Expr = {
      p match {
        case (e1, Nil) => e1
        case (e1, (op, e2) :: rest) => Prim (op, e1, foldAssocRight (e2, rest))
      }
    }

    // NOTE: this has the wrong behavior!
    val p05 : Parser[Expr] = P (integer ~ (("+" | "-").! ~ integer).rep.map (s => s.toList)).map (foldAssocRight)
    testEval (p05, "12")
    testEval (p05, "12+345")
    testEval (p05, "12+345-5")
    testEval (p05, "8-3-1")
    println ("=" * 80)

    // NOTE: this has the wrong behavior!
    val p06 : Parser[Expr] = P (integer ~ (("+" | "-" | "*" ).! ~ integer).rep.map (s => s.toList)).map (foldAssocLeft)
    testEval (p06, "12")
    testEval (p06, "12+345")
    testEval (p06, "12*345+5")
    testEval (p06, "12+345*5")
    println ("=" * 80)

    val p07a : Parser[Expr] = P (integer ~ ("*".! ~ integer).rep.map (s => s.toList)).map (foldAssocLeft)
    val p07b : Parser[Expr] = P (p07a ~ (("+" | "-").! ~ p07a).rep.map (s => s.toList)).map (foldAssocLeft)
    val p07 : Parser[Expr] = p07b
    testEval (p07, "12")
    testEval (p07, "12+345")
    testEval (p07, "12*345+5")
    testEval (p07, "12+345*5")
    println ("=" * 80)

    object O8 {
      // Putting declarations inside an object allows them to be mutually recursive.
      val parens : Parser[Expr] = P (integer | ("(" ~ addSub ~ ")"))
      val mult : Parser[Expr] = P (parens ~ ("*".! ~ parens).rep.map (s => s.toList)).map (foldAssocLeft)
      val addSub : Parser[Expr] = P (mult ~ (("+" | "-").! ~ mult).rep.map (s => s.toList)).map (foldAssocLeft)
    }
    val p08 : Parser[Expr] = O8.addSub
    testEval (p08, "12")
    testEval (p08, "12+345")
    testEval (p08, "12*345+5")
    testEval (p08, "12+345*5")
    testEval (p08, "(12+345)*5")
    testEval (p08, "(((12+345)))*5")
    testEval (p08, "(((12+345)))* 5")   // watch out!
    testEval (p08, "(((12+345)) )*5")
    println ("=" * 80)
  }
}