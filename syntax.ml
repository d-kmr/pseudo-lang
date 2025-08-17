(*
alphabet ::= a | .. | z | A | .. | Z
digit ::= 0 | .. | 9
pdigit ::= 1 | .. | 9
varsym :: = _ | -
identifier ::= <alphabet>(<alphabet>|<digit>|<varsym>)*
num ::= <pdigit>
field ::= <identifier>

i ::= <identifier>
n ::= <num>
e ::= 
    | n | i | null
	| e+e | e-e | -e | e*e | e/e | e mod e // arithmetical expression
	| e.field // member access
	| e[e]  // array access
	| i(e,..,e) // function call
c ::= 
    | e=e | e!=e | e<e | e>e | e<=e | e>=e 
    | c and c | c or c | not c
    | true | false
stmt ::=
    | x <- e // assignment
    | if c then <suite> (else if c then <suite>)* [else <suite>] end if
    | for each i in e <suite> end for
    | for i from e to e <suite> end for
    | while c do <suite> end while
    | i(e,..,e)

fundef ::= function i(i,..,i) <suite> end function

suite ::= <indent>stmt<newline>(<suite>)*

keywords = [ return,print,for,each,while,if,then,else,in,from,to,end,and,or,not,mod,function,null,true,false ]
*)
open Tools
module Fmt = Format
           
module Exp = struct

  type t =
    | Num of int | Var of string | Null
    | Add of t * t | Sub of t * t | Minus of t | Mul of t * t | Div of t * t | Mod of t * t
    | Mem of t * string (* e.field *)
    | Idx of t * t (* e[e] *)
    | Lst of t list (* [e1,e2,e3] *)
    | Fun of string * t list (* f(e1,e2) *)

  let rec pp fmt e =  (* pretty printer *)
    match e with
    | Num n -> Fmt.fprintf fmt "%d" n
    | Var v -> Fmt.fprintf fmt "%s" v
    | Null -> Fmt.fprintf fmt "null"
    | Add(e1,e2) -> Fmt.fprintf fmt "%a + %a" ppi e1 ppi e2
    | Sub(e1,e2) -> Fmt.fprintf fmt "%a - %a" ppi e1 ppi e2
    | Minus e -> Fmt.fprintf fmt "-%a" ppi e
    | Mul(e1,e2) -> Fmt.fprintf fmt "%a * %a" ppi e1 ppi e2
    | Div(e1,e2) -> Fmt.fprintf fmt "%a / %a" ppi e1 ppi e2
    | Mod(e1,e2) -> Fmt.fprintf fmt "%a mod %a" ppi e1 ppi e2
    | Mem(e,field) -> Fmt.fprintf fmt "%a.%s" ppi e field
    | Idx(e1,e2) -> Fmt.fprintf fmt "%a[%a]" ppi e1 pp e2
    | Lst ee -> Fmt.fprintf fmt "[%a]" (pp_list_comma pp) ee
    | Fun(fname,ee) -> Fmt.fprintf fmt "%s(%a)" fname (pp_list_comma pp) ee
  and ppi fmt e = (* pretty printer for innier expression *)
    match e with
    | Add(e1,e2) -> Fmt.fprintf fmt "(%a + %a)" ppi e1 ppi e2
    | Sub(e1,e2) -> Fmt.fprintf fmt "(%a - %a)" ppi e1 ppi e2
    | Minus e -> Fmt.fprintf fmt "(-%a)" ppi e
    | Mul(e1,e2) -> Fmt.fprintf fmt "(%a * %a)" ppi e1 ppi e2
    | Div(e1,e2) -> Fmt.fprintf fmt "(%a / %a)" ppi e1 ppi e2
    | Mod(e1,e2) -> Fmt.fprintf fmt "(%a mod %a)" ppi e1 ppi e2  
    | _ -> pp fmt e
      
  let to_python e = Fmt.asprintf "@[%a@]" pp e
                  
end
;;
module E = Exp
;;

module Cond = struct
  
  type t =
    | Eq of E.t * E.t | Neq of E.t * E.t
    | Lt of E.t * E.t | Gt of E.t * E.t
    | Le of E.t * E.t | Ge of E.t * E.t
    | And of t * t | Or of t * t | Not of t
    | True | False
           
  let rec pp fmt c =  (* pretty printer *)
    match c with
    | Eq(e1,e2) -> Fmt.fprintf fmt "%a = %a" E.ppi e1 E.ppi e2
    | Neq(e1,e2) -> Fmt.fprintf fmt "%a != %a" E.ppi e1 E.ppi e2
    | Lt(e1,e2) -> Fmt.fprintf fmt "%a < %a" E.ppi e1 E.ppi e2
    | Gt(e1,e2) -> Fmt.fprintf fmt "%a > %a" E.ppi e1 E.ppi e2
    | Le(e1,e2) -> Fmt.fprintf fmt "%a <= %a" E.ppi e1 E.ppi e2
    | Ge(e1,e2) -> Fmt.fprintf fmt "%a >= %a" E.ppi e1 E.ppi e2                 
    | And(c1,c2) -> Fmt.fprintf fmt "%a and %a" ppi c1 ppi c2
    | Or(c1,c2) -> Fmt.fprintf fmt "%a or %a" ppi c1 ppi c2
    | Not c -> Fmt.fprintf fmt "not %a" ppi c
    | True -> Fmt.fprintf fmt "true"
    | False -> Fmt.fprintf fmt "false"
  and ppi fmt c = (* pretty printer for innier expression *)
    match c with
    | Eq(e1,e2) -> Fmt.fprintf fmt "(%a = %a)" E.ppi e1 E.ppi e2
    | Neq(e1,e2) -> Fmt.fprintf fmt "(%a != %a)" E.ppi e1 E.ppi e2
    | Lt(e1,e2) -> Fmt.fprintf fmt "(%a < %a)" E.ppi e1 E.ppi e2
    | Gt(e1,e2) -> Fmt.fprintf fmt "(%a > %a)" E.ppi e1 E.ppi e2
    | Le(e1,e2) -> Fmt.fprintf fmt "(%a <= %a)" E.ppi e1 E.ppi e2
    | Ge(e1,e2) -> Fmt.fprintf fmt "(%a >= %a)" E.ppi e1 E.ppi e2                 
    | And(c1,c2) -> Fmt.fprintf fmt "(%a and %a)" ppi c1 ppi c2
    | Or(c1,c2) -> Fmt.fprintf fmt "(%a or %a)" ppi c1 ppi c2
    | Not c -> Fmt.fprintf fmt "(not %a)" ppi c
    | _ -> pp fmt c

  let to_python c = Fmt.asprintf "@[%a@]" pp c
                  
end
;;
module C = Cond
;;

module Stmt = struct

  type t =
    | Asgn of string * Exp.t (* x<-e *)
    | If of Cond.t * t list * (Cond.t * t list) list * t list option
    (* if c then <suite> (else if c then <suite>)* [else <suite>] end if *)
    | Foreach of string * Exp.t * t list (* for each i in e <suite> end for *)
    | For of string * Exp.t * Exp.t * t list (* for i from e to e <suite> end for *)
    | While of Cond.t * t list (* while c do <suite> end while *)
    | Fcall of string * Exp.t list (* f(e,..,e) *)
    | Return of Exp.t option
    | Print of Exp.t

  let indent_skip = 2 (* indent skip length *)

  let indent n = String.make (n*indent_skip) ' '
             
  let rec pp d fmt stmt =  (* pretty printer with indent (d:indent depth) *)
    match stmt with
    | Asgn(v,e) ->
       Fmt.fprintf fmt "%s@[%s = %a@." (indent d) v Exp.pp e
    | If(c,stmtL,elifL,elseOpt) ->
       Fmt.fprintf fmt "%s@[if %a:\n" (indent d) Cond.pp c;
       Fmt.fprintf fmt "%a" (pp_suite (d+1)) stmtL;
       Fmt.fprintf fmt "%a" (pp_list_none (pp_elif d)) elifL;
       Fmt.fprintf fmt "%a@]" (pp_opt (pp_else d)) elseOpt
    | Foreach(v,e,stmtL) ->
       Fmt.fprintf fmt "%s@[foreach %s in %a:\n" (indent d) v Exp.pp e;
       Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
    | For(v,eStart,eEnd,stmtL) ->
       Fmt.fprintf fmt "%s@[for %s from %a to %a:\n" (indent d) v Exp.pp eStart Exp.pp eEnd;
       Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
    | While(c,stmtL) ->
       Fmt.fprintf fmt "%s@[while %a:\n" (indent d) Cond.pp c;
       Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
    | Fcall(fname,ee) ->
       Fmt.fprintf fmt "%s@[%s(%a)@." (indent d) fname (pp_list_comma Exp.pp) ee
    | Return None ->
       Fmt.fprintf fmt "%s@[return@." (indent d)
    | Return (Some e) ->
       Fmt.fprintf fmt "%s@[return(%a)@." (indent d) Exp.pp e
    | Print e ->
       Fmt.fprintf fmt "%s@[print(%a)@." (indent d) Exp.pp e
  and pp_elif d fmt (c,stmtL) =
    Fmt.fprintf fmt "%s@[elif %a:\n" (indent d) Cond.pp c;
    Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
  and pp_else d fmt stmtL =
    Fmt.fprintf fmt "%s@[else:\n" (indent d);
    Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
  and pp_suite d fmt stmtL =
    Fmt.fprintf fmt "%a" (pp_list_none (pp d)) stmtL

  let to_python stmt = Fmt.asprintf "@[%a@]" (pp 0) stmt
    
end
;;

module FunDef = struct

  type t = FunDef of string * string list * Stmt.t list

  let pp fmt (FunDef(fname,prmL,ssBody)) =
    Fmt.fprintf fmt "@[def %s(%a):\n" fname (pp_list_comma pp_string) prmL;
    Fmt.fprintf fmt "%a@." (Stmt.pp_suite 1) ssBody

  let to_python fdef = Fmt.asprintf "@[%a@]" pp fdef
    
end
;;

module Program = struct

  type t = FunDef.t list

  let pp fmt (prog:t) =
    Fmt.fprintf fmt "@[%a" (pp_list_newline FunDef.pp) prog

  let to_python prog = Fmt.asprintf "@[%a@]" pp prog

end
;;



module ShortCut = struct 
  let _IF(c,ssThen,uuElseIf,ssElse) = Stmt.If(c,ssThen,uuElseIf,ssElse)
  let _WHILE(c,ss) = Stmt.While(c,ss)
  let _FOREACH(v,e,ss) = Stmt.Foreach(v,e,ss)
  let _FOR(v,e1,e2,ss) = Stmt.For(v,e1,e2,ss)
  let _FCALL(f,ee) = Stmt.Fcall(f,ee)
  let _RETURN0 = Stmt.Return None
  let _RETURN e = Stmt.Return (Some e)
  let ( <.- ) v e = Stmt.Asgn(v,e)
  let ( =.= ) e1 e2 = Cond.Eq(e1,e2)
  let ( <.> ) e1 e2 = Cond.Neq(e1,e2)
  let ( +.+ ) e1 e2 = Exp.Add(e1,e2)
  let ( -.- ) e1 e2 = Exp.Sub(e1,e2)
  let _LST ee = Exp.Lst ee
  let _True = Cond.True
  let _False = Cond.False
  let num n = Exp.Num n
  let n0,n1,n2,n3,n4,n5 = num 0,num 1,num 2,num 3,num 4,num 5
  let xx,yy = Exp.Var "x",Exp.Var "y"
          
end
;;

open ShortCut
       
let s1 = _IF(_True,["x" <.- n1; "y" <.- n2;_FCALL("f",[n4])],[(xx =.= yy,["x" <.- n5]);(xx =.= yy,["x" <.- n5])],None);;
let s2 = _FOREACH("z",_LST[n0;n1;n2],[s1;_RETURN0]);;
Fmt.printf "%a" (Stmt.pp 0) s2;;
