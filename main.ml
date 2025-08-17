(*
**  Qsitory
**
*)
open Syntax
open Tools   
module F = Format

(* read from file *)
let inputstr_file filename =
  let x = ref "" in
  let ic = open_in filename in
  try
	while true do
	  x := !x ^ (input_line ic) ^ "\n"
	done ;
	"" (* dummy *)
  with
  | End_of_file -> close_in ic;!x
  | _ -> print_endline "Exception: No File..."; exit 0

(* parser *)
let parse str =
  let cache =
    let l = ref [] in
    fun lexbuf ->
    match !l with
    | x::xs -> l := xs; x
    | [] ->
       match Lexer.tokens lexbuf with
       | [] -> failwith ""
       | x::xs -> l := xs; x
  in
  let lexbuf = Lexing.from_string str in
  Lexer.inIndent := Lexer.nextCharIs [' ';'\t'] lexbuf;
  try
    doIfDebug "LEXING" print_endline ">> Input";
    doIfDebug "LEXING" (F.printf "@[%S@.") str;
    doIfDebug "LEXING" print_endline ">> Lexed Result";
    clearMemo ();
    let e = Parser.main cache lexbuf in
    doIfDebug "LEXING" (F.printf "@[%s@.") !tokenMemo;
    e
  with
  | Parsing.Parse_error ->
     doIfDebug "LEXING" (F.printf "@[%s@.") !tokenMemo;
     F.printf "@[\n\nParse error: %S@." (Lexing.lexeme lexbuf);
     exit 0
  | ParseError mes ->
     doIfDebug "LEXING" (F.printf "@[%s@.") !tokenMemo;
     F.printf "@[\n\nParse error: %S@." mes;
     exit 0
  | _ ->
     doIfDebug "LEXING" (F.printf "@[%s@.") !tokenMemo;
     F.printf "@[\n\nUnknown Parse error: %S@." (Lexing.lexeme lexbuf);
     exit 0
;;

let prog:Program.t ref = ref []
;;
let readfile filename =
  prog := parse (inputstr_file filename)
;;         
let msgUsage = "USAGE: spyc <file>.spy"

let speclist = [
    ("-lex", Arg.Unit (fun _ -> addDebugOpt "LEXING"), "Set lexing debug mode");
    ("-parse", Arg.Unit (fun _ -> addDebugOpt "PARSING"), "Set parsing debug mode");
]

let () =
  if Array.length Sys.argv < 2 then (print_endline msgUsage; exit 0);
  Arg.parse speclist readfile msgUsage;
  print_endline (Program.to_python !prog)
;;

