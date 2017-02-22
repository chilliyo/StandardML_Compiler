package csp.ch03

// From SBT: ~run-main csp.ch03.Project

object Project {

   ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Syntax
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr
  
  sealed trait Stmt
  case class Asgn (nm : String, e : Expr)                                     extends Stmt
  case class If (e1 : Expr, e2 : Expr, s2 : Stmt)                             extends Stmt
  case class Block (ss : List[Stmt])                                          extends Stmt
  case class For (nm : String, low : Expr, high : Expr, s : Stmt)             extends Stmt
  case class While (e : Expr, s : Stmt)                                       extends Stmt
  case class Print (e : Expr)                                                 extends Stmt
  case class Function (nm1 : String, nm2 : String, e: Expr, ss : List[Stmt])  extends Stmt
  case class RecursiveCall(e1: Expr, e2: Expr)                                extends Stmt

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Parsing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("let", "in", "end", "val", "if", "then","else", "while", "do", "for", "print","fun")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val variable : Parser[Expr] = ident.map (s => Var (s))
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace ((" "| "\n").rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val atExpr : Parser[Expr] = P (
      integer |
      variable |
      ("(" ~/ expr ~ ")")
    )
    val typeParser: Parser[Expr] = P ( 
      (":" ~ multDiv)
    )

    val string: Parser[Expr] = P ( 
      (atExpr ~ (((" ").rep).! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )

    val multDiv : Parser[Expr] = P (
      (atExpr ~ (("*" | "/").! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val addSub : Parser[Expr] = P (
      (multDiv ~ (("+" | "-" | "%").! ~ multDiv).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val gtLtGeLeExpr : Parser[Expr] = P (
      (addSub ~ ((">" | "<" | ">=" | "<=").! ~ addSub).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val eqNeExpr : Parser[Expr] = P (
      (gtLtGeLeExpr ~ (("=" | "<>").! ~ gtLtGeLeExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )

    val expr : Parser[Expr] = P (eqNeExpr)

    val stmt : Parser[Stmt] = P (
      
      (ident ~ ":=" ~ expr ~ ";").map { case (nm, e) => Asgn (nm, e) } |
      ("if" ~ "(" ~ expr ~ ")" ~ "then" ~ atExpr ~ "else" ~ stmt ~ ";").map { case (e1, e2, s2) => If (e1, e2, s2) } |
      ("(" ~ stmt.rep ~ ")").map { case ss => Block (ss.toList) } |
      ("for" ~ "(" ~ ident ~ ":=" ~ expr ~ "to" ~ expr ~ ")" ~ stmt).map { case (nm, e1, e2, s) => For (nm, e1, e2, s) } |
      ("while" ~  expr ~ "do" ~ stmt).map { case (e, s) => While (e, s) } |
      ("print" ~ "('" ~ string ~ "')" ~ ";").map { case (e) => Print (e) } |
      ("fun" ~ ident ~ "(" ~ ident ~ typeParser ~ ")" ~ "="  ~ stmt.rep ).map {case (nm1, nm2, e, ss) => Function(nm1, nm2, e, ss.toList) } |
      (multDiv ~ "(" ~ addSub ~ ")" ).map {case (e1, e2) => RecursiveCall(e1, e2) }
    )


    val start : Parser[Stmt] = P (stmt ~ End)
    // val atom : Parser[Unit] = P (integer.map(s => ()) | variable.map(s => ()))

    // val assignStatment: Parser[Unit] = P(ident.map(s =>()) ~ ":=" ~ integer.map (n => ()) ~ ";")

    // val assignStatments: Parser[Unit] = P(assignStatment.rep)

    // val whileStatment: P[Unit] = 
    //   P ("while" ~ "(" ~ ident.map(s=> ()) ~ ")" ~ "do" ~ "(" ~ assignStatments ~ ")" ~ ";")

    // val printStatement: P[Unit] = 
    //   P ("print" ~ "(" ~ "'" ~ alpha ~ " ".? ~ alpha ~  ")")

    // val statments: P[Unit] = 
    //   P (
    //     assignStatment |
    //     whileStatment
    //     )

    // val parameter : P[Unit] = P (ident.map(s => ()))

    // val parameters: P[Unit] = P ( (parameter ~ ",").rep ~ parameter)

    // val functionDec : P[Unit] = 
    //   P ("fun" ~ ident.map(s => ()) ~ "(" ~ parameters ~ ")" ~ "=" ~ "(" ~ statments ~ ")")

    // val start : Parser[Unit] = P (functionDec ~ End)
  }

 
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
//      case If (e1, e2, e3)  => if (eval (e1,env) != 0)  eval(e2,env)
//                  else eval (e3,env)
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")

    }
  }

  import fastparse.all.{Parsed,Parser}

  def test (p : Parser[Stmt], s : String) : Unit = {
    val result : fastparse.core.Parsed[Stmt, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (stmt, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, stmt, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[Stmt] = MyParsers.start

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //                Assignment Test: Function, assignment statment, expressions, type declearation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    test (p01, "fun max(q:Int) = q:= 0;")


   ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //               While loop Test: Function, while-loop statment, expressions, type declearation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    test (p01, """fun foo(a:Int) = 
                    while x<9 do
                      a := a*9; 
                      b := 100;
                  """
          )


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //                Hello World Test: Function, print statment, expressions, type declearation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //testing: main method with a print statment.
     test(p01, """fun main(n:String) = 

                      print('Hello World');

                  """)



  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //                Factorial Test: Function, if-else statment, expressions, type declearation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      test(p01, "if (n<1) then (1) else (n*fac(n-1));")
      test(p01, """fun factorial(n:Int) = 
                                        if (n<1)
                                        then (1) 
                                        else (n*fac(n-1));
                                      
                """)
    println ("=" * 80)
  }
}