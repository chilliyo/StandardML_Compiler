package csp.ch01


object ExpressionsVariables {
  
  sealed trait Expr
  case class CstI (n:Int)                       extends Expr
  case class Var (s:String)                     extends Expr
  case class Prim (op:String, e1:Expr, e2:Expr) extends Expr
  case class If   (e1:Expr, e2:Expr, e3:Expr)   extends Expr

  type Env = List[(String, Int)]

  val emptyenv : Env = Nil

  def lookup (env:Env, x:String) : Int = {
    env match {
      case Nil           => throw new RuntimeException (x + " not found")
      case ((y, v) :: r) => if (x == y) v else lookup (r, x)
    }
  }

  def eval (env:Env, e:Expr) : Int = {
    e match {
      case CstI (n)           => n
      case Var (x)            => lookup (env, x)
      case Prim ("+", e1, e2) => eval (env, e1) + eval (env, e2)
      case Prim ("*", e1, e2) => eval (env, e1) * eval (env, e2)
      case Prim ("-", e1, e2) => eval (env, e1) - eval (env, e2)
      case Prim ("max", e1, e2) => val v1 = eval(env, e1)
                     val v2 = eval(env, e2)
                    if (v1 >= v2) v1 else v2
      case Prim ("min", e1, e2) => val v1 = eval(env, e1)
                       val v2 = eval(env, e2)
                    if (v1 <= v2) v1 else v2
      case Prim ("==", e1, e2) => if (eval(env, e1) == eval(env, e2)) 1 else 0
      case If (e1, e2, e3)  => if (eval (env, e1) != 0)  eval(env, e2)
                  else eval (env, e3)
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }
  
  def main(args: Array[String]) : Unit = {
      println(eval (List ( ("a", 5) ), If(Var("a"), Prim ("max", CstI (7), CstI (2)) , CstI(22))))
      println(eval (List ( ("a", 0) ), If(Var("a"), CstI(11), Prim ("min", CstI (10), CstI (30)))))
      println(eval (List ( ("a", 5) ), If(Var("a"), CstI(1), CstI(4))))
  }
}