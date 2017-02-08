package csp.ch03

// From SBT: ~run-main csp.ch03.Project

object Project {
  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("let", "in", "end", "val")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val variable : Parser[Expr] = ident.map (s => Var (s))
    
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace ((" " | "\n").rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val atom : Parser[Unit] = P (integer.map(s => ()) | variable.map(s => ()))

    val assignStatment: Parser[Unit] = P(ident.map(s =>()) ~ ":=" ~ integer.map (n => ()) ~ ";")

    val assignStatments: Parser[Unit] = P(assignStatment.rep)

    val whileStatment: P[Unit] = 
      P ("while" ~ "(" ~ ident.map(s=> ()) ~ ")" ~ "do" ~ "(" ~ assignStatments ~ ")" ~ ";")

    val statments: P[Unit] = 
      P (
        assignStatment |
        whileStatment
        )

    val parameter : P[Unit] = P (ident.map(s => ()))

    val parameters: P[Unit] = P ( (parameter ~ ",").rep ~ parameter)

    val functionDec : P[Unit] = 
      P ("fun" ~ ident.map(s => ()) ~ "(" ~ parameters ~ ")" ~ "=" ~ "(" ~ statments ~ ")")

    val start : Parser[Unit] = P (functionDec ~ End)
  }

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr
  case class If(e1:Expr, e2:Expr, e3:Expr)            extends Expr

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
      case Let (x, erhs, ebody) => {
        val xval : Int = eval (erhs, env)
        val env1 : List[(String, Int)] = (x, xval) :: env 
        eval (ebody, env1)
      }
      case Prim ("+", e1, e2) => eval (e1, env) + eval (e2, env)
      case Prim ("*", e1, e2) => eval (e1, env) * eval (e2, env)
      case Prim ("-", e1, e2) => eval (e1, env) - eval (e2, env)
      case If (e1, e2, e3)  => if (eval (e1,env) != 0)  eval(e2,env)
                  else eval (e3,env)
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")

    }
  }

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  import fastparse.all.{Parsed,Parser}

  def test (p : Parser[Unit], s : String) : Unit = {
    val result : fastparse.core.Parsed[Unit, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Unit] = MyParsers.start
    //test (MyParsers.statment, "x = 5;")
    //test (MyParsers.atom, "a")
    //test (p01, "fun double(x) = x;")

    //testing: Function Decleration with assignment statement(s).
    test (p01, "fun max(q) = (q:= 0;)")
    test (p01, "fun double(x,y,z) = (x := 2;)")
    //testing: Function Decleration with assignment and while-loop statement(s).
    test (p01, """fun foo(a,b) = (
                    while (x) do (
                      a := 9; 
                      b := 100;
                    );
                  )"""
          )

    println ("=" * 80)
  }
}