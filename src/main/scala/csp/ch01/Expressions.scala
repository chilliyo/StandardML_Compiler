package csp.ch01


object Expressions {
  
  sealed trait Expr
  case class CstI (n:Int)                       extends Expr
  case class Var  (s:String)                     extends Expr
  case class Prim (op:String, e1:Expr, e2:Expr) extends Expr
  case class If   (e1:Expr, e2:Expr, e3:Expr) 	extends Expr


type Env = List[(String, Int)]

val emptyenv : Env = Nil

def lookup (env:Env, x:String) : Int = {
  env match {
    case Nil           => throw new RuntimeException (x + " not found")
    case ((y, v) :: r) => if (x == y) v else lookup (r, x)
  }
}

// Excerise 1.1 (i)&(ii)
  def eval (e:Expr) : Int = {
    e match {
      case CstI (n)           => n
      case Prim ("+", e1, e2) => eval (e1) + eval (e2)
      case Prim ("*", e1, e2) => eval (e1) * eval (e2)
      case Prim ("-", e1, e2) => eval (e1) - eval (e2)
      case Prim ("max", e1, e2) => val v1 = eval(e1)
      							   val v2 = eval(e2)
      							if (v1 >= v2) v1 else v2
      case Prim ("min", e1, e2) => val v1 = eval(e1)
      							   val v2 = eval(e2)
      							if (v1 <= v2) v1 else v2
      case Prim ("==", e1, e2) => if (eval(e1) == eval(e2)) 1 else 0
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }

  def evalm (e:Expr) : Int = {
    e match {
      case CstI (n)           => n
      case Prim ("+", e1, e2) => evalm (e1) + evalm (e2)
      case Prim ("*", e1, e2) => evalm (e1) * evalm (e2)
      case Prim ("-", e1, e2) => {
        val res = evalm (e1) - evalm (e2)
        if (res < 0) 0 else res
      }
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }

// Excerise 1.1 (iii)
  def eval2 (e:Expr) : Int = {
    e match {
      case CstI (n)          => n
      case Prim (op, e1, e2) =>
      	val v1 = eval2 (e1)
      	val v2 = eval2 (e2) 

    	op match {
      		case "+" => v1 + v2
        	case "*" => v1 * v2
        	case "-" => v1 - v2
        	case "max" => if(v1 >= v2) v1 else v2
        	case "min" => if(v1 <= v2) v1 else v2
        	case "==" => if(v1 == v2) 1 else 0
        	case   _ => throw new RuntimeException ("unknown primitive")
  	  	}	
    }
  }

// Excerise 1.1 (iv) & (v)
  def eval3 (env:Env, e:Expr) : Int = {
  e match {
    case CstI (n)           => n
    case Var (x)            => lookup (env, x)
    case Prim ("+", e1, e2) => eval3 (env, e1) + eval3 (env, e2)
    case Prim ("*", e1, e2) => eval3 (env, e1) * eval3 (env, e2)
    case Prim ("-", e1, e2) => eval3 (env, e1) - eval3 (env, e2)
    case Prim ("max", e1, e2) => val v1 = eval3(env, e1)
      							 val v2 = eval3(env, e2)
      							if (v1 >= v2) v1 else v2
    case Prim ("min", e1, e2) => val v1 = eval3(env, e1)
      							 val v2 = eval3(env, e2)
      							if (v1 <= v2) v1 else v2
    case Prim ("==", e1, e2) => if (eval3(env, e1) == eval3(env, e2)) 1 else 0
    case If (e1, e2, e3)	=> if (eval3 (env, e1) != 0)  eval3(env, e2)
    							else eval3 (env, e3)
    case Prim (  _,  _,  _) => 
      throw new RuntimeException ("unknown primitive")
  }
}


  
  def main (args: Array[String]) : Unit = {
  	println(eval (Prim ("max", CstI (1), Prim ("max", CstI (7), CstI (2)))))
  	println(eval (Prim ("min", CstI (20), Prim ("min", CstI (10), CstI (30)))))
  	println(eval (Prim ("==", CstI(4), CstI(3))))
  	println(eval (Prim ("==", CstI(3), CstI(3))))

  	println(eval2 (Prim ("max", CstI (1), Prim ("max", CstI (7), CstI (2)))))
  	println(eval2 (Prim ("min", CstI (20), Prim ("min", CstI (10), CstI (30)))))
  	println(eval2 (Prim ("==", CstI(4), CstI(3))))
  	println(eval2 (Prim ("==", CstI(3), CstI(3))))

  	println(eval3 (List ( ("a", 5) ), If(Var("a"), Prim ("max", CstI (7), CstI (2)) , CstI(22))))
  	println(eval3 (List ( ("a", 0) ), If(Var("a"), CstI(11), Prim ("min", CstI (10), CstI (30)))))

  }
}