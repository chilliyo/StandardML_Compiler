package csp.ch02

// (* Programming language concepts for software developers, 2012-02-17 *)
//
// (* Evaluation, checking, and compilation of object language expressions *)
// (* Stack machines for expression evaluation                             *) 
//
// (* Object language expressions with variable bindings and nested scope *)

object Intcomp1 {

  // type expr = 
  //   | CstI of int
  //   | Var of string
  //   | Let of string * expr * expr
  //   | Prim of string * expr * expr;;

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Let (nm : String, e1 : Expr, e2 : Expr)  extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr

  // (* Some closed expressions: *)
  //
  // let e1 = Let("z", CstI 17, Prim("+", Var "z", Var "z"));;

  val e1 : Expr = Let ("z", CstI (17), Prim ("+", Var ("z"), Var ("z")))

  // let e2 = Let("z", CstI 17, 
  //              Prim("+", Let("z", CstI 22, Prim("*", CstI 100, Var "z")),
  //                        Var "z"));;

  val e2 : Expr = Let ("z", CstI (17), 
                       Prim("+", Let ("z", CstI (22), Prim ("*", CstI (100), Var ("z"))),
                            Var ("z")))

  // let e3 = Let("z", Prim("-", CstI 5, CstI 4), 
  //              Prim("*", CstI 100, Var "z"));;

  val e3 : Expr = Let ("z", Prim ("-", CstI (5), CstI (4)), 
                       Prim ("*", CstI (100), Var ("z")))

  // let e4 = Prim("+", Prim("+", CstI 20, Let("z", CstI 17, 
  //                                           Prim("+", Var "z", CstI 2))),
  //                    CstI 30);;

  val e4 : Expr = Prim ("+", Prim ("+", CstI (20), Let ("z", CstI (17), 
                                                        Prim ("+", Var ("z"), CstI (2)))),
                        CstI (30))

  // let e5 = Prim("*", CstI 2, Let("x", CstI 3, Prim("+", Var "x", CstI 4)));;
  
  val e5 : Expr = Prim ("*", CstI (2), Let ("x", CstI (3), Prim ("+", Var ("x"), CstI (4))))

  // (* Evaluation of expressions with variables and bindings *)

  // let rec lookup env x =
  //     match env with 
  //     | []        -> failwith (x + " not found")
  //     | (y, v)::r -> if x=y then v else lookup r x;;

  def lookup (env : List[(String, Int)], x : String) : Int = {
    env match {
      case Nil         => throw new RuntimeException (x + " not found")
      case (y, v) :: r => if (x == y) v else lookup (r, x)
    }
  }

  // let rec eval e (env : (string * int) list) : int =
  //     match e with
  //     | CstI i            -> i
  //     | Var x             -> lookup env x 
  //     | Let(x, erhs, ebody) -> 
  //       let xval = eval erhs env
  //       let env1 = (x, xval) :: env 
  //       eval ebody env1
  //     | Prim("+", e1, e2) -> eval e1 env + eval e2 env
  //     | Prim("*", e1, e2) -> eval e1 env * eval e2 env
  //     | Prim("-", e1, e2) -> eval e1 env - eval e2 env
  //     | Prim _            -> failwith "unknown primitive";;

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
      case Prim (  _,  _,  _) => throw new RuntimeException ("unknown primitive")
    }
  }

  // let run e = eval e [];;

  def run (e : Expr) = eval (e, Nil)

  // (* ---------------------------------------------------------------------- *)

  // (* Closedness *)

  // // let mem x vs = List.exists (fun y -> x=y) vs;;

  def mem [X] (x : X, vs : List[X]) : Boolean = vs.contains (x)

  // (* Checking whether an expression is closed.  The vs is 
  //    a list of the bound variables.  *)

  // let rec closedin (e : expr) (vs : string list) : bool =
  //     match e with
  //     | CstI i -> true
  //     | Var x  -> List.exists (fun y -> x=y) vs
  //     | Let(x, erhs, ebody) -> 
  //       let vs1 = x :: vs 
  //       closedin erhs vs && closedin ebody vs1
  //     | Prim(ope, e1, e2) -> closedin e1 vs && closedin e2 vs;;

  def closedin (e : Expr, vs : List[String]) : Boolean = {
    e match {
      case CstI (i) => true
      case Var (x)  => vs.contains (x)
      case Let (x, erhs, ebody) => {
        val vs1 : List[String] = x :: vs 
        closedin (erhs, vs) && closedin (ebody, vs1)
      }
      case Prim (ope, e1, e2) => closedin (e1, vs) && closedin (e2, vs)
    }
  }

  // (* An expression is closed if it is closed in the empty environment *)

  // let closed1 e = closedin e [];;

  def closed1 (e : Expr) = closedin (e, Nil)

  // (* ---------------------------------------------------------------------- *)

  // (* Substitution of expressions for variables *)

  // (* This version of lookup returns a Var(x) expression if there is no
  //    pair (x,e) in the list env --- instead of failing with exception: *)

  // let rec lookOrSelf env x =
  //     match env with 
  //     | []        -> Var x
  //     | (y, e)::r -> if x=y then e else lookOrSelf r x;;

  def lookOrSelf (env : List[(String, Expr)], x : String) : Expr = {
    env match {
      case Nil       => Var (x)
      case (y, e)::r => if (x == y) e else lookOrSelf (r, x)
    }
  }

  // (* Remove (x, _) from env: *)

  // let rec remove env x =
  //     match env with 
  //     | []        -> []
  //     | (y, e)::r -> if x=y then r else (y, e) :: remove r x;;

  def remove (env : List[(String, Expr)], x : String) : List[(String, Expr)] = {
    env match {
      case Nil        => Nil
      case (y, e):: r => if (x == y) r else (y, e) :: remove (r, x)
    }
  }

  // (* Naive substitution, may capture free variables: *)

  // let rec nsubst (e : expr) (env : (string * expr) list) : expr =
  //     match e with
  //     | CstI i -> e
  //     | Var x  -> lookOrSelf env x
  //     | Let(x, erhs, ebody) ->
  //       let newenv = remove env x
  //       Let(x, nsubst erhs env, nsubst ebody newenv)
  //     | Prim(ope, e1, e2) -> Prim(ope, nsubst e1 env, nsubst e2 env)

  def nsubst (e : Expr, env : List[(String, Expr)]) : Expr = {
    e match {
      case CstI (i)           => e
      case Var (x)            => lookOrSelf (env, x)
      case Let (x, erhs, ebody) => {
        val newenv : List[(String, Expr)] = remove (env, x)
        Let (x, nsubst (erhs, env), nsubst (ebody, newenv))
      }
      case Prim (ope, e1, e2) => Prim (ope, nsubst (e1, env), nsubst (e2, env))
    }
  }

  // (* Some expressions with free variables: *)

  // let e6 = Prim("+", Var "y", Var "z");;

  val e6 : Expr = Prim ("+", Var ("y"), Var ("z"))

  // let e6s1 = nsubst e6 [("z", CstI 17)];;

  val e6s1 : Expr = nsubst (e6, List ( ("z", CstI (17)) ))
  
  // let e6s2 = nsubst e6 [("z", Prim("-", CstI 5, CstI 4))];;

  val e6s2 : Expr = nsubst (e6, List ( ("z", Prim ("-", CstI (5), CstI (4))) ))

  // let e6s3 = nsubst e6 [("z", Prim("+", Var "z", Var "z"))];;

  val e6s3 : Expr = nsubst (e6, List ( ("z", Prim ("+", Var ("z"), Var ("z"))) ))

  // // Shows that only z outside the Let gets substituted:
  // let e7 = Prim("+", Let("z", CstI 22, Prim("*", CstI 5, Var "z")),
  //                    Var "z");;

  val e7 : Expr = Prim ("+", Let ("z", CstI (22), Prim ("*", CstI (5), Var ("z"))),
                        Var ("z"))

  // let e7s1 = nsubst e7 [("z", CstI 100)];;

  val e7s1 : Expr = nsubst (e7, List ( ("z", CstI (100)) ))

  // // Shows that only the z in the Let rhs gets substituted
  // let e8 = Let("z", Prim("*", CstI 22, Var "z"), Prim("*", CstI 5, Var "z"));;

  val e8 : Expr = Let ("z", Prim ("*", CstI (22), Var ("z")), Prim ("*", CstI (5), Var ("z")))

  // let e8s1 = nsubst e8 [("z", CstI 100)];;

  val e8s1 : Expr = nsubst (e8, List ( ("z", CstI (100)) ))

  // // Shows (wrong) capture of free variable z under the let:
  // let e9 = Let("z", CstI 22, Prim("*", Var "y", Var "z"));;

  val e9 : Expr = Let ("z", CstI (22), Prim ("*", Var ("y"), Var ("z")))

  // let e9s1 = nsubst e9 [("y", Var "z")];;

  val e9s1 : Expr = nsubst (e9, List ( ("y", Var ("z")) ))

  // let e9s2 = nsubst e9 [("z", Prim("-", CstI 5, CstI 4))];;

  val e9s2 : Expr = nsubst (e9, List ( ("z", Prim ("-", CstI (5), CstI (4))) ))

  // let newVar : string -> string = 
  //     let n = ref 0
  //     let varMaker x = (n := 1 + !n; x + string (!n))
  //     varMaker

  val newVar : String => String = {
    var n : Int = 0
    def varMaker (x : String) : String = {
      n = 1 + n
      x + n
    }
    varMaker
  }

  // (* Correct, capture-avoiding substitution *)

  // let rec subst (e : expr) (env : (string * expr) list) : expr =
  //     match e with
  //     | CstI i -> e
  //     | Var x  -> lookOrSelf env x
  //     | Let(x, erhs, ebody) ->
  //       let newx = newVar x
  //       let newenv = (x, Var newx) :: remove env x
  //       Let(newx, subst erhs env, subst ebody newenv)
  //     | Prim(ope, e1, e2) -> Prim(ope, subst e1 env, subst e2 env)

  def subst (e : Expr, env : List[(String, Expr)]) : Expr = {
    e match {
      case CstI (i) => e
      case Var (x)  => lookOrSelf (env, x)
      case Let (x, erhs, ebody) => {
        val newx : String = newVar (x)
        val newenv : List[(String, Expr)] = (x, Var (newx)) :: remove (env, x)
        Let (newx, subst (erhs, env), subst (ebody, newenv))
      }
      case Prim (ope, e1, e2) => Prim (ope, subst (e1, env), subst (e2, env))
    }
  }

  // let e6s1a = subst e6 [("z", CstI 17)];;

  val vale6s1a : Expr = subst (e6, List ( ("z", CstI (17)) ))

  // let e6s2a = subst e6 [("z", Prim("-", CstI 5, CstI 4))];;

  val e6s2a : Expr = subst (e6, List ( ("z", Prim ("-", CstI (5), CstI (4))) ))

  // let e6s3a = subst e6 [("z", Prim("+", Var "z", Var "z"))];;

  val e6s3a : Expr = subst (e6, List ( ("z", Prim ("+", Var ("z"), Var ("z"))) ))

  // // Shows renaming of bound variable z (to z1)
  // let e7s1a = subst e7 [("z", CstI 100)];;

  val e7s1a : Expr = subst (e7, List ( ("z", CstI (100)) ))

  // // Shows renaming of bound variable z (to z2)
  // let e8s1a = subst e8 [("z", CstI 100)];;

  val e8s1a : Expr = subst (e8, List ( ("z", CstI (100)) ))

  // // Shows renaming of bound variable z (to z3), avoiding capture of free z
  // let e9s1a = subst e9 [("y", Var "z")];;

  val e9s1a : Expr = subst (e9, List ( ("y", Var ("z")) ))

  // (* ---------------------------------------------------------------------- *)

  // (* Free variables *)

  // (* Operations on sets, represented as lists.  Simple but inefficient;
  //    one could use binary trees, hashtables or splaytrees for
  //    efficiency.  *)

  // (* union(xs, ys) is the set of all elements in xs or ys, without duplicates *)

  // let rec union (xs, ys) = 
  //     match xs with 
  //     | []    -> ys
  //     | x::xr -> if mem x ys then union(xr, ys)
  //                else x :: union(xr, ys);;
  
  def union [A] (xs : List[A], ys : List[A]) : List[A] = {
    xs match {
      case Nil   => ys
      case x::xr => if (mem (x, ys)) {
        union (xr, ys) 
      } else {
        x :: union (xr, ys)
      }
    }
  }

  // (* minus xs ys  is the set of all elements in xs but not in ys *)

  // let rec minus (xs, ys) = 
  //     match xs with 
  //     | []    -> []
  //     | x::xr -> if mem x ys then minus(xr, ys)
  //                else x :: minus (xr, ys);;

  def minus [A] (xs : List[A], ys : List[A]) : List[A] = {
    xs match {
      case Nil   => Nil
      case x::xr => if (mem (x, ys)) minus (xr, ys) else x :: minus (xr, ys)
    }
  }

  // (* Find all variables that occur free in expression e *)

  // let rec freevars e : string list =
  //     match e with
  //     | CstI i -> []
  //     | Var x  -> [x]
  //     | Let(x, erhs, ebody) -> 
  //           union (freevars erhs, minus (freevars ebody, [x]))
  //     | Prim(ope, e1, e2) -> union (freevars e1, freevars e2);;

  def freevars (e : Expr) : List[String] = {
    e match {
      case CstI (i) => Nil
      case Var (x) => List (x)
      case Let (x, erhs, ebody) => union (freevars (erhs), minus (freevars (ebody), List (x)))
      case Prim (ope, e1, e2) => union (freevars (e1), freevars (e2))
    }
  }
  
  /*
    CSC 348(Winter 2017)
    Expression with Local Variable
    
    For each of the following expressions: 
    work out what the free variables are by hand; 
    convert it to abstract syntax; 
    add your expression as a val definition to Intcomp1.scala
    
    let x = y + 1 in x + 1
    let x = x + 1 in x + 1
    let x = (let x = y + 1 in x + 1) in x + 1
    let x = (let y = 1 in y + 1) in x + y
  */

  //let x = y + 1 in x + 1
  //Free and Bound Variables: let x = y(free) + 1 in x(bound) + 1

  val fvex1: Expr = Let("x",
                        Prim("+", Var("y"), CstI(1)),
                        Prim("+", Var("x"), CstI(1))
                        )

  //let x = x + 1 in x + 1
  //Free and Bound Variables: let x = x(free) + 1 in x(bound) + 1
  val fvex2: Expr = Let("x",
                        Prim("+", Var("x"), CstI(1)),
                        Prim("+", Var("x"), CstI(1))
                        )  

  //let x = (let x = y + 1 in x + 1) in x + 1
  /*Free and Bound Variables:
    let x = (let x = y(free) + 1 in x(bound) + 1) in x(bound) + 1
  */

  val fvex3: Expr = Let("x",
                        Let("x",
                            Prim("+", Var("y"), CstI(1)),
                            Prim("+", Var("y"), CstI(1))
                            ),
                        Prim("+", Var("x"), CstI(1))
                        )
  
  //let x = (let y = 1 in y + 1) in x + y
  /*Free and Bound Variables:
    let x = (let y = 1 in y(bound) + 1) in x(bound) + y(free)
  */

  val fvex4: Expr = Let("x",
                        Let("y",
                            CstI(1),
                            Prim("+", Var("y"), CstI(1))
                           ),
                        Prim("+", Var("x"), Var("y"))
                        )
                            
  freevars(fvex4)

  def main(args: Array[String]) : Unit = {
    println("Free variable of (let x = y + 1 in x + 1): " + freevars(fvex1))
    println("Free variable of (let x = x + 1 in x + 1): " + freevars(fvex2))
    println("Free variable of (let x = (let x = y + 1 in x + 1) in x + 1): " + freevars(fvex3))
    println("Free variable of (let x = (let y = 1 in y + 1) in x + y): " + freevars(fvex4))
  }

  // (* Alternative definition of closed *)

  // let closed2 e = (freevars e = []);;

  def closed2 (e : Expr) : Boolean = freevars (e) == Nil


  // (* ---------------------------------------------------------------------- *)

  // (* Compilation to target expressions with numerical indexes instead of
  //    symbolic variable names.  *)

  // type texpr =                            (* target expressions *)
  //   | TCstI of int
  //   | TVar of int                         (* index into runtime environment *)
  //   | TLet of texpr * texpr               (* erhs and ebody                 *)
  //   | TPrim of string * texpr * texpr;;

  sealed trait TExpr                    // target expressions
  case class TCstI (n : Int)                             extends TExpr
  case class TVar (n : Int)                              extends TExpr // index into runtime environment
  case class TLet (e1 : TExpr, e2 : TExpr)               extends TExpr // erhs and ebody
  case class TPrim (nm : String, e1 : TExpr, e2 : TExpr) extends TExpr

  // (* Map variable name to variable index at compile-time *)

  // let rec getindex vs x = 
  //     match vs with 
  //     | []    -> failwith "Variable not found"
  //     | y::yr -> if x=y then 0 else 1 + getindex yr x;

  def getindex [X] (vs : List[X], x : X) : Int = {
    vs match {
      case Nil   => throw new RuntimeException ("Variable not found")
      case y::yr => if (x == y) 0 else 1 + getindex (yr, x)
    }
  }

  // (* Compiling from expr to texpr *)

  // let rec tcomp (e : expr) (cenv : string list) : texpr =
  //     match e with
  //     | CstI i -> TCstI i
  //     | Var x  -> TVar (getindex cenv x)
  //     | Let(x, erhs, ebody) -> 
  //       let cenv1 = x :: cenv 
  //       TLet(tcomp erhs cenv, tcomp ebody cenv1)
  //     | Prim(ope, e1, e2) -> TPrim(ope, tcomp e1 cenv, tcomp e2 cenv);;

  def tcomp (e : Expr, cenv : List[String]) : TExpr = {
    e match {
      case CstI (i)             => TCstI (i)
      case Var (x)              => TVar (getindex (cenv, x))
      case Let (x, erhs, ebody) => 
        val cenv1 = x :: cenv 
        TLet (tcomp (erhs, cenv), tcomp (ebody, cenv1))
      case Prim (ope, e1, e2)   => TPrim (ope, tcomp (e1, cenv), tcomp (e2, cenv));;
    }
  }

  // (* Evaluation of target expressions with variable indexes.  The
  //    run-time environment renv is a list of variable values (ints).  *)

  // let rec teval (e : texpr) (renv : int list) : int =
  //     match e with
  //     | TCstI i -> i
  //     | TVar n  -> List.nth renv n
  //     | TLet(erhs, ebody) -> 
  //       let xval = teval erhs renv
  //       let renv1 = xval :: renv 
  //       teval ebody renv1 
  //     | TPrim("+", e1, e2) -> teval e1 renv + teval e2 renv
  //     | TPrim("*", e1, e2) -> teval e1 renv * teval e2 renv
  //     | TPrim("-", e1, e2) -> teval e1 renv - teval e2 renv
  //     | TPrim _            -> failwith "unknown primitive";;

  def teval (e : TExpr, renv : List[Int]) : Int = {
      e match {
        case TCstI (i) => i
        case TVar (n)  => renv (n)
        case TLet (erhs, ebody) => 
          val xval = teval (erhs, renv)
          val renv1 = xval :: renv 
          teval (ebody, renv1)
        case TPrim ("+", e1, e2) => teval (e1, renv) + teval (e2, renv)
        case TPrim ("*", e1, e2) => teval (e1, renv) * teval (e2, renv)
        case TPrim ("-", e1, e2) => teval (e1, renv) - teval (e2, renv)
        case TPrim (  _,  _,  _) => throw new RuntimeException ("Unknown primitive")
      }
  }

  // (* Correctness: eval e []  equals  teval (tcomp e []) [] *)


  // (* ---------------------------------------------------------------------- *)

  // (* Stack machines *)

  // (* Stack machine instructions.  An expressions in postfix or reverse
  //    Polish form is a list of stack machine instructions. *)

  // type rinstr =
  //   | RCstI of int
  //   | RAdd 
  //   | RSub
  //   | RMul 
  //   | RDup
  //   | RSwap;;

  sealed trait RInstr
  case class RCstI (n : Int) extends RInstr
  case object RAdd           extends RInstr
  case object RSub           extends RInstr
  case object RMul           extends RInstr
  case object RDup           extends RInstr
  case object RSwap          extends RInstr

  // (* A simple stack machine for evaluation of variable-free expressions
  //    in postfix form *)

  // let rec reval (inss : rinstr list) (stack : int list) : int =
  //     match (inss, stack) with 
  //     | ([], v :: _) -> v
  //     | ([], [])     -> failwith "reval: no result on stack!"
  //     | (RCstI i :: insr,             stk)  -> reval insr (i::stk)
  //     | (RAdd    :: insr, i2 :: i1 :: stkr) -> reval insr ((i1+i2)::stkr)
  //     | (RSub    :: insr, i2 :: i1 :: stkr) -> reval insr ((i1-i2)::stkr)
  //     | (RMul    :: insr, i2 :: i1 :: stkr) -> reval insr ((i1*i2)::stkr)
  //     | (RDup    :: insr,       i1 :: stkr) -> reval insr (i1 :: i1 :: stkr)
  //     | (RSwap   :: insr, i2 :: i1 :: stkr) -> reval insr (i1 :: i2 :: stkr)
  //     | _ -> failwith "reval: too few operands on stack";;

  def reval (inss : List[RInstr], stack : List[Int]) : Int = {
    (inss, stack) match {
      case (Nil, v :: _)                         => v
      case (Nil, Nil)                            => throw new RuntimeException ("reval: no result on stack!")
      case (RCstI (i) :: insr,             stk)  => reval (insr, (i::stk))
      case (RAdd      :: insr, i2 :: i1 :: stkr) => reval (insr, ((i1+i2)::stkr))
      case (RSub      :: insr, i2 :: i1 :: stkr) => reval (insr, ((i1-i2)::stkr))
      case (RMul      :: insr, i2 :: i1 :: stkr) => reval (insr, ((i1*i2)::stkr))
      case (RDup      :: insr,       i1 :: stkr) => reval (insr, (i1 :: i1 :: stkr))
      case (RSwap     :: insr, i2 :: i1 :: stkr) => reval (insr, (i1 :: i2 :: stkr))
      case _                                     => throw new RuntimeException ("reval: too few operands on stack")
    }
  }

  // let rpn1 = reval [RCstI 10; RCstI 17; RDup; RMul; RAdd] [];;

  val rpn1 : Int = reval (List (RCstI (10), RCstI (17), RDup, RMul, RAdd), List ())

  // (* Compilation of a variable-free expression to a rinstr list *)

  // let rec rcomp (e : expr) : rinstr list =
  //     match e with
  //     | CstI i            -> [RCstI i]
  //     | Var _             -> failwith "rcomp cannot compile Var"
  //     | Let _             -> failwith "rcomp cannot compile Let"
  //     | Prim("+", e1, e2) -> rcomp e1 @ rcomp e2 @ [RAdd]
  //     | Prim("*", e1, e2) -> rcomp e1 @ rcomp e2 @ [RMul]
  //     | Prim("-", e1, e2) -> rcomp e1 @ rcomp e2 @ [RSub]
  //     | Prim _            -> failwith "unknown primitive";;
  
  def rcomp (e : Expr) : List[RInstr] = {
    e match {
      case CstI (i)           => List (RCstI (i))
      case Var (_)            => throw new RuntimeException ("rcomp cannot compile Var")
      case Let (_, _, _)      => throw new RuntimeException ("rcomp cannot compile Let")
      case Prim ("+", e1, e2) => rcomp (e1) ::: rcomp (e2) ::: List (RAdd)
      case Prim ("*", e1, e2) => rcomp (e1) ::: rcomp (e2) ::: List (RMul)
      case Prim ("-", e1, e2) => rcomp (e1) ::: rcomp (e2) ::: List (RSub)
      case Prim (_, _, _)     => throw new RuntimeException ("unknown primitive")
    }
  }

  // (* Correctness: eval e []  equals  reval (rcomp e) [] *)


  // (* Storing intermediate results and variable bindings in the same stack *)

  // type sinstr =
  //   | SCstI of int                        (* push integer           *)
  //   | SVar of int                         (* push variable from env *)
  //   | SAdd                                (* pop args, push sum     *)
  //   | SSub                                (* pop args, push diff.   *)
  //   | SMul                                (* pop args, push product *)
  //   | SPop                                (* pop value/unbind var   *)
  //   | SSwap;;                             (* exchange top and next  *)

  sealed trait SInstr
  case class SCstI (n : Int) extends SInstr // push integer           
  case class SVar (n : Int)  extends SInstr // push variable from env 
  case object SAdd           extends SInstr // pop args, push sum     
  case object SSub           extends SInstr // pop args, push diff.   
  case object SMul           extends SInstr // pop args, push product 
  case object SPop           extends SInstr // pop value/unbind var   
  case object SSwap          extends SInstr // exchange top and next  

  // let rec seval (inss : sinstr list) (stack : int list) =
  //     match (inss, stack) with
  //     | ([], v :: _) -> v
  //     | ([], [])     -> failwith "seval: no result on stack"
  //     | (SCstI i :: insr,          stk) -> seval insr (i :: stk) 
  //     | (SVar i  :: insr,          stk) -> seval insr (List.nth stk i :: stk) 
  //     | (SAdd    :: insr, i2::i1::stkr) -> seval insr (i1+i2 :: stkr)
  //     | (SSub    :: insr, i2::i1::stkr) -> seval insr (i1-i2 :: stkr)
  //     | (SMul    :: insr, i2::i1::stkr) -> seval insr (i1*i2 :: stkr)
  //     | (SPop    :: insr,    _ :: stkr) -> seval insr stkr
  //     | (SSwap   :: insr, i2::i1::stkr) -> seval insr (i1::i2::stkr)
  //     | _ -> failwith "seval: too few operands on stack";;

  def seval (inss : List[SInstr], stack : List[Int]) : Int = {
    (inss, stack) match {
      case (Nil, v :: _)                     => v
      case (Nil, Nil)                        => throw new RuntimeException ("seval: no result on stack")
      case (SCstI (i) :: insr,          stk) => seval (insr, (i :: stk))
      case (SVar (i)  :: insr,          stk) => seval (insr, (stk (i) :: stk))
      case (SAdd      :: insr, i2::i1::stkr) => seval (insr, (i1+i2 :: stkr))
      case (SSub      :: insr, i2::i1::stkr) => seval (insr, (i1-i2 :: stkr))
      case (SMul      :: insr, i2::i1::stkr) => seval (insr, (i1*i2 :: stkr))
      case (SPop      :: insr,    _ :: stkr) => seval (insr, stkr)
      case (SSwap     :: insr, i2::i1::stkr) => seval (insr, (i1::i2::stkr))
      case _                                 => throw new RuntimeException ("seval: too few operands on stack")
    }
  }

  // (* A compile-time variable environment representing the state of
  //    the run-time stack. *)

  // type stackvalue =
  //   | Value                               (* A computed value *)
  //   | Bound of string;;                   (* A bound variable *)

  sealed trait StackValue
  case object Value             extends StackValue // A computed value
  case class Bound (s : String) extends StackValue // A bound variable
   
  // (* Compilation to a list of instructions for a unified-stack machine *)

  // let rec scomp (e : expr) (cenv : stackvalue list) : sinstr list =
  //     match e with
  //     | CstI i -> [SCstI i]
  //     | Var x  -> [SVar (getindex cenv (Bound x))]
  //     | Let(x, erhs, ebody) -> 
  //           scomp erhs cenv @ scomp ebody (Bound x :: cenv) @ [SSwap; SPop]
  //     | Prim("+", e1, e2) -> 
  //           scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SAdd] 
  //     | Prim("-", e1, e2) -> 
  //           scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SSub] 
  //     | Prim("*", e1, e2) -> 
  //           scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SMul] 
  //     | Prim _ -> failwith "scomp: unknown operator";;

  def scomp (e : Expr, cenv : List[StackValue]) : List[SInstr] = {
    e match {
      case CstI (i) => List (SCstI (i))
      case Var (x)  => List (SVar (getindex (cenv, (Bound (x)))))
      case Let (x, erhs, ebody) => 
        scomp (erhs, cenv) ::: scomp (ebody, (Bound (x) :: cenv)) ::: List (SSwap, SPop)
      case Prim ("+", e1, e2) => 
        scomp (e1, cenv) ::: scomp (e2, (Value :: cenv)) ::: List (SAdd)
      case Prim ("-", e1, e2) => 
        scomp (e1, cenv) ::: scomp (e2, (Value :: cenv)) ::: List (SSub)
      case Prim ("*", e1, e2) => 
        scomp (e1, cenv) ::: scomp (e2, (Value :: cenv)) ::: List (SMul)
      case Prim (_, _, _) => throw new RuntimeException ("scomp: unknown operator")
      case _ => Nil
    }
  }

  // let s1 = scomp e1 [];;
  // let s2 = scomp e2 [];;
  // let s3 = scomp e3 [];;
  // let s5 = scomp e5 [];;

  val s1 : List[SInstr] = scomp (e1, Nil)
  val s2 : List[SInstr] = scomp (e2, Nil)
  val s3 : List[SInstr] = scomp (e3, Nil)
  val s5 : List[SInstr] = scomp (e5, Nil)

  // (* Output the integers in list inss to the text file called fname: *)

  // let intsToFile (inss : int list) (fname : string) = 
  //     let text = String.concat " " (List.map string inss)
  //     System.IO.File.WriteAllText(fname, text);;

  def intsToFile (inss : List[Int], fname : String) = {
    val text : StringBuilder = inss.addString (new StringBuilder, " ")
    println (fname + text)
  }

}
