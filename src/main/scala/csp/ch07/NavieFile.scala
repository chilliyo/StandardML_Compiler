package csp.ch07

// From SBT: ~run-main csp.ch07.NaiveFile

// Based on the parser/interpreter in directory Imp from the Sestoft source code.

// Demonstrates how to read input from a file rather than a string.

object NaiveFile {

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Syntax
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Expr
  case class CstI (n : Int)                                           extends Expr
  case class Var (nm : String)                                        extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr)                 extends Expr

  sealed trait Stmt
  case class Asgn (nm : String, e : Expr)                         extends Stmt
  case class If (e : Expr, s1 : Stmt, s2 : Stmt)                  extends Stmt
  case class Block (ss : List[Stmt])                              extends Stmt
  case class For (nm : String, low : Expr, high : Expr, s : Stmt) extends Stmt
  case class While (e : Expr, s : Stmt)                           extends Stmt
  case class Print (e : Expr)                                     extends Stmt

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Parsing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil)              => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = P (digits.map (n => CstI (n)))

    val keywords : List[String] = List ("if", "else", "while", "for", "print")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val variable : Parser[Expr] = P (ident.map (s => Var (s)))
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (CharIn (" \n").rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val atExpr : Parser[Expr] = P (
      integer |
      variable |
      ("(" ~/ expr ~ ")")
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
      ("if" ~ "(" ~ expr ~ ")" ~ stmt ~ "else" ~ stmt).map { case (e, s1, s2) => If (e, s1, s2) } |
      ("{" ~ stmt.rep ~ "}").map { case ss => Block (ss.toList) } |
      ("for" ~ "(" ~ ident ~ ":=" ~ expr ~ "to" ~ expr ~ ")" ~ stmt).map { case (nm, e1, e2, s) => For (nm, e1, e2, s) } |
      ("while" ~ "(" ~ expr ~ ")" ~ stmt).map { case (e, s) => While (e, s) } |
      ("print" ~ "(" ~ expr ~ ")" ~ ";").map { case (e) => Print (e) }
    )

    val start : Parser[Stmt] = P (stmt ~ End)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Pretty printing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def ppExpr (e : Expr) : String = {
    e match {
      case CstI (i)                     => i.toString
      case Var (x)                      => x
      case Prim (op, e1, e2)            => "(%s %s %s)".format (ppExpr (e1), op, ppExpr (e2))
    }
  }

  def ppBlock (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
      case Block (ss) => {
        val sb = new StringBuilder
        for (s <- ss) {
          sb.append (ppStmt (newIndent, s))
        }
        sb.toString
      }
      case _ => {
        "%s".format (ppStmt (newIndent, s))
      }
    }
  }

  def ppStmt (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
      case Asgn (nm, e)           =>
        "%s%s := %s;\n".format (indent, nm, ppExpr (e))
      case If (e, s1, s2)         =>
        "%sif (%s) {\n%s%s} else {\n%s%s}\n".format (indent, ppExpr (e), ppBlock (indent, s1), indent, ppBlock (indent, s2), indent)
      case Block (ss) => {
        "%s{\n%s%s}\n".format (indent, ppBlock (indent, s), indent)
      }
      case For (nm, low, high, s) => {
        "%sfor (%s := %s to %s) {\n%s%s}\n".format (indent, nm, ppExpr (low), ppExpr (high), ppBlock (indent, s), indent)
      }
      case While (e, s)           =>
        "%swhile (%s) {\n%s%s}\n".format (indent, ppExpr (e), ppBlock (indent, s), indent)
      case Print (e)              =>
        "%sprint (%s);\n".format (indent, ppExpr (e))
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Evaluation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // (* A naive store is a map from names (strings) to values (ints) *)

  type NaiveStore = Map[String,Int]

  val emptyStore : NaiveStore = Map.empty

  def getSto (store : NaiveStore, x : String) : Int = {
    store.get (x).get
  }

  def setSto (store : NaiveStore, k : String, v : Int) : NaiveStore = {
    store + ( (k, v) )
  }

  def b2i (b : Boolean) : Int = if (b) 1 else 0

  def eval (e : Expr, store : NaiveStore) : Int = {
    e match {
      case CstI (i)           => i
      case Var (x)            => getSto (store, x)
      case Prim (op, e1, e2) => {
        val i1 = eval (e1, store)
        val i2 = eval (e2, store)
        op match {
          case  "+" => i1 + i2
          case  "-" => i1 - i2
          case  "*" => i1 * i2
          case "==" => b2i (i1 == i2)
          case "<>" => b2i (i1 != i2)
          case  "<" => b2i (i1 < i2)
          case  ">" => b2i (i1 > i2)
          case "<=" => b2i (i1 <= i2)
          case ">=" => b2i (i1 >= i2)
          case   _ => throw new RuntimeException ("unknown primitive " + op)
        }
      }
    }
  }

  def exec (s : Stmt, store : NaiveStore) : NaiveStore = {
    s match {
      case Asgn (nm, e)            => {
        val v : Int = eval (e, store)
        // println ("store is %s".format (store))
        // println ("assigning %d to %s".format (v, nm))
        setSto (store, nm, v)
      }
      case If (e, s1, s2)          => exec (if (eval (e, store) != 0) s1 else s2, store)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt], store2 : NaiveStore) : NaiveStore = {
          ss2 match {
            case Nil       => store2
            case s2 :: ss3 => loop (ss3, exec (s2, store2))
          }
        }
        loop (ss, store)
      }
      case For (nm, low, high, s)  => {
        val start : Int = eval (low, store)
        val stop : Int = eval (high, store)
        def loop (i : Int, sto : NaiveStore) : NaiveStore = {
          if (i > stop) {
            sto
          } else {
            loop (i + 1, exec (s, setSto (sto, nm, i)))
          }
        }
        loop (start, store)
      }
      case While (e, s)            => {
        def loop (sto : NaiveStore) : NaiveStore = {
          if (eval (e, sto) != 0) {
            loop (exec (s, sto))
          } else {
            sto
          }
        }
        loop (store)
      }
      case Print (e)               => {
        println (eval (e, store))
        store
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Testing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def run (s : Stmt) : Unit = {
    exec (s, emptyStore)
  }

  // (* Example programs *)

  val ex1 : Stmt = {
    Block (
      List (
        Asgn (
          "sum",
          CstI (0)),
        For (
          "i",
          CstI (0),
          CstI (100),
          Asgn (
            "sum",
            Prim ("+", Var ("sum"), Var ("i"))
          )),
        Print (Var ("sum"))
      )
    )
  }

  val ex2 : Stmt = {
    Block (
      List (
        Asgn (
          "i",
          CstI (1)
        ),
        Asgn (
          "sum",
          CstI (0)
        ),
        While (
          Prim ("<", Var ("sum"), CstI (10000)),
          Block (
            List (
              Print (Var ("sum")),
              Asgn (
                "sum",
                Prim ("+", Var ("sum"), Var ("i"))
              ),
              Asgn (
                "i",
                Prim ("+", CstI (1), Var ("i"))
              )
            )
          )
        ),
        Print (Var ("i")),
        Print (Var ("sum"))
      )
    )
  }

  val ex3 : Stmt = {
    Block (
      List (
        If (
          CstI (0),
          Block (
            List (
              Print (Var ("sum"))
            )
          ),
          Block (
            List (
              Print (Var ("sum"))
            )
          )
        ),
        If (
          CstI (0),
          Print (Var ("sum")),
          Block (
            List (
              Print (Var ("sum"))
            )
          )
        ),
        If (
          CstI (0),
          Block (
            List (
              Print (Var ("sum"))
            )
          ),
          Print (Var ("sum"))
        ),
        If (
          CstI (0),
          If (
            CstI (0),
            Block (
              List (
                Print (Var ("sum"))
              )
            ),
            Print (Var ("sum"))
          ),
          Print (Var ("sum"))
        ),
        For (
          "i",
          CstI (0),
          CstI (100),
          Asgn (
            "sum",
            Prim ("+", Var ("sum"), Var ("i"))
          )),
        Print (Var ("sum"))
      )
    )
  }

  import fastparse.all.{Parsed,Parser}

  def test (p : Parser[Stmt], filename : String) : Unit = {
    val source : scala.io.BufferedSource = io.Source.fromFile (filename)
    val input : String = try source.getLines.mkString ("\n") finally source.close ()
    val result : fastparse.core.Parsed[Stmt, Char, String] = p.parse (input)
    result match {
      case Parsed.Success (stmt, successIndex) => {
        println ("Successfully parsed file \"%s\".\nResult is %s.\nIndex is %d.".format (filename, stmt, successIndex))
        println ("Pretty printing:")
        print (ppStmt ("  ", stmt))
        println ("Running:")
        run (stmt)
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse file \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (filename, lastParser, index, extra))
      }
    }
  }


  def main (args : Array[String]) {
    println ("=" * 80)

    // run (ex1)
    // println (ppStmt ("", ex1))
    // println ("=" * 80)

    // run (ex2)
    // println (ppStmt ("", ex2))
    // println ("=" * 80)

    // println (ppStmt ("", ex3))
    // println ("=" * 80)

    import java.io.File
    for (f <- new File ("./input/ch07-naive").listFiles.toList.sortWith ((f1, f2) => f1.getName < f2.getName)) {
      test (MyParsers.start, f.getPath)
      println ("=" * 80)
    }
  }
}