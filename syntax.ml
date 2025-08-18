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
    // arithmetical expression
    | n | i | null
	| e+e | e-e | -e | e*e | e/e | e mod e 
	| e.field // member access
	| e[e]  // array access
	| i(e,..,e) // function call
    | { fld=e,.., fld=e } // struct
    | [e,..,e] // array
    // boolean expression
    | e=e | e!=e | e<e | e>e | e<=e | e>=e 
    | e and e | e or e | not e
    | true | false
stmt ::=
    | e <- e // assignment
    | if e then <suite> (else if e then <suite>)* [else <suite>] end if
    | for each i in e <suite> end for
    | for i from e to e <suite> end for
    | while e do <suite> end while
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
    | Idx of t * t (* e[e]   a[3]  --> Idx(Var "a", Num 3) *)
    | Lst of t list (* [e1,e2,e3] *)
    | Fun of string * t list (* f(e1,e2) *)
    | Str of (string * t) list
    | Eq of t * t | Neq of t * t
    | Lt of t * t | Gt  of t * t
    | Le of t * t | Ge  of t * t
    | And of t * t | Or of t * t | Not of t
    | True | False

  let rec pp fmt e =  (* pretty printer *)
    match e with
    | Num n -> Fmt.fprintf fmt "%d" n
    | Var v -> Fmt.fprintf fmt "%s" v
    | Null -> Fmt.fprintf fmt "None"
    | Add(e1,e2) -> Fmt.fprintf fmt "%a + %a" ppi e1 ppi e2
    | Sub(e1,e2) -> Fmt.fprintf fmt "%a - %a" ppi e1 ppi e2
    | Minus e -> Fmt.fprintf fmt "-%a" ppi e
    | Mul(e1,e2) -> Fmt.fprintf fmt "%a * %a" ppi e1 ppi e2
    | Div(e1,e2) -> Fmt.fprintf fmt "%a / %a" ppi e1 ppi e2
    | Mod(e1,e2) -> Fmt.fprintf fmt "%a mod %a" ppi e1 ppi e2
    | Mem(e,field) -> Fmt.fprintf fmt "%a[\'%s\']" ppi e field
    | Idx(e1,e2) -> Fmt.fprintf fmt "%a[%a]" ppi e1 pp e2
    | Lst ee -> Fmt.fprintf fmt "[%a]" (pp_list_comma pp) ee
    | Fun(fname,ee) -> Fmt.fprintf fmt "%s(%a)" fname (pp_list_comma pp) ee
    | Str ff -> Fmt.fprintf fmt "{%a}" (pp_list_comma pp_struct_content) ff
    | Eq(e1,e2) -> Fmt.fprintf fmt "%a == %a" ppi e1 ppi e2
    | Neq(e1,e2) -> Fmt.fprintf fmt "%a != %a" ppi e1 ppi e2
    | Lt(e1,e2) -> Fmt.fprintf fmt "%a < %a" ppi e1 ppi e2
    | Gt(e1,e2) -> Fmt.fprintf fmt "%a > %a" ppi e1 ppi e2
    | Le(e1,e2) -> Fmt.fprintf fmt "%a <= %a" ppi e1 ppi e2
    | Ge(e1,e2) -> Fmt.fprintf fmt "%a >= %a" ppi e1 ppi e2
    | And(e1,e2) -> Fmt.fprintf fmt "%a and %a" ppi e1 ppi e2
    | Or(e1,e2) -> Fmt.fprintf fmt "%a or %a" ppi e1 ppi e2
    | Not e -> Fmt.fprintf fmt "not %a" ppi e
    | True -> Fmt.fprintf fmt "True"
    | False -> Fmt.fprintf fmt "False"
  and ppi fmt e = (* pretty printer for innier expression *)
    match e with
    | Add(e1,e2) -> Fmt.fprintf fmt "(%a + %a)" ppi e1 ppi e2
    | Sub(e1,e2) -> Fmt.fprintf fmt "(%a - %a)" ppi e1 ppi e2
    | Minus e -> Fmt.fprintf fmt "(-%a)" ppi e
    | Mul(e1,e2) -> Fmt.fprintf fmt "(%a * %a)" ppi e1 ppi e2
    | Div(e1,e2) -> Fmt.fprintf fmt "(%a / %a)" ppi e1 ppi e2
    | Mod(e1,e2) -> Fmt.fprintf fmt "(%a mod %a)" ppi e1 ppi e2  
    | Eq(e1,e2) -> Fmt.fprintf fmt "(%a = %a)" ppi e1 ppi e2
    | Neq(e1,e2) -> Fmt.fprintf fmt "(%a != %a)" ppi e1 ppi e2
    | Lt(e1,e2) -> Fmt.fprintf fmt "(%a < %a)" ppi e1 ppi e2
    | Gt(e1,e2) -> Fmt.fprintf fmt "(%a > %a)" ppi e1 ppi e2
    | Le(e1,e2) -> Fmt.fprintf fmt "(%a <= %a)" ppi e1 ppi e2
    | Ge(e1,e2) -> Fmt.fprintf fmt "(%a >= %a)" ppi e1 ppi e2                 
    | And(e1,e2) -> Fmt.fprintf fmt "(%a and %a)" ppi e1 ppi e2
    | Or(e1,e2) -> Fmt.fprintf fmt "(%a or %a)" ppi e1 ppi e2
    | Not e -> Fmt.fprintf fmt "(not %a)" ppi e
    | _ -> pp fmt e
  and pp_struct_content fmt (fld,e) = Fmt.fprintf fmt "\'%s\':%a" fld pp e
      
  let to_python e = Fmt.asprintf "@[%a@]" pp e
                  
end
;;
module E = Exp
;;

module Stmt = struct

  type t =
    | Asgn of Exp.t * Exp.t (* x<-e or a[n]<-e *)
    | If of Exp.t * t list * (Exp.t * t list) list * t list option
    (* if e then <suite> (else if e then <suite>)* [else <suite>] end if *)
    | Foreach of string * Exp.t * t list (* for each i in e <suite> end for *)
    | For of string * Exp.t * Exp.t * t list (* for i from e to e <suite> end for *)
    | While of Exp.t * t list (* while e do <suite> end while *)
    | Fcall of string * Exp.t list (* f(e,..,e) *)
    | Return of Exp.t option
    | Print of Exp.t

  let indent_skip = 2 (* indent skip length *)

  let indent n = String.make (n*indent_skip) ' '
             
  let rec pp d fmt stmt =  (* pretty printer with indent (d:indent depth) *)
    match stmt with
    | Asgn(e1,e2) ->
       Fmt.fprintf fmt "%s@[<h>%a = %a@." (indent d) Exp.pp e1 Exp.pp e2
    | If(e,stmtL,elifL,elseOpt) ->
       Fmt.fprintf fmt "%s@[<h>if %a:\n" (indent d) Exp.pp e;
       Fmt.fprintf fmt "%a" (pp_suite (d+1)) stmtL;
       Fmt.fprintf fmt "%a" (pp_list_none (pp_elif d)) elifL;
       Fmt.fprintf fmt "%a@]" (pp_opt (pp_else d)) elseOpt
    | Foreach(v,e,stmtL) ->
       Fmt.fprintf fmt "%s@[<h>foreach %s in %a:\n" (indent d) v Exp.pp e;
       Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
    | For(v,eStart,eEnd,stmtL) ->
       Fmt.fprintf fmt "%s@[<h>for %s in range(%a,%a+1):\n" (indent d) v Exp.pp eStart Exp.pp eEnd;
       Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
    | While(c,stmtL) ->
       Fmt.fprintf fmt "%s@[<h>while %a:\n" (indent d) Exp.pp c;
       Fmt.fprintf fmt "%a@]" (pp_suite (d+1)) stmtL
    | Fcall(fname,ee) ->
       Fmt.fprintf fmt "%s@[<h>%s(%a)@." (indent d) fname (pp_list_comma Exp.pp) ee
    | Return None ->
       Fmt.fprintf fmt "%s@[<h>return@." (indent d)
    | Return (Some e) ->
       Fmt.fprintf fmt "%s@[<h>return(%a)@." (indent d) Exp.pp e
    | Print e ->
       Fmt.fprintf fmt "%s@[<h>print(%a)@." (indent d) Exp.pp e
  and pp_elif d fmt (e,stmtL) =
    Fmt.fprintf fmt "%s@[elif %a:\n" (indent d) Exp.pp e;
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

  type t = FD of string * string list * Stmt.t list (* function f(x,y) <suite> end function *)

  let pp fmt (FD(fname,prmL,ssBody)) = 
    Fmt.fprintf fmt "@[def %s(%a):\n" fname (pp_list_comma pp_string) prmL;
    Fmt.fprintf fmt "%a@]" (Stmt.pp_suite 1) ssBody

  let to_python fdef = Fmt.asprintf "@[%a@]" pp fdef
    
end
;;

module Program = struct

  type t = FunDef.t list

  let pp fmt prog = Fmt.fprintf fmt "@[%a@]" (pp_list_newline FunDef.pp) prog
                  
  let to_python prog = Fmt.asprintf "@[%a\n\n%s@." pp prog "main()"

end
;;
