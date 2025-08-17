// Parser for Spy-lang
%{
  open Syntax
  open Tools
%}

%token <string> IDENT  // x, y, Abc, a_b-c
%token <int> INT
%token INDENT
%token DEDENT
%token TRUE     // "true"           
%token FALSE    // "false"
%token WHILE    // "while"
%token FOR      // "for"
%token EACH     // "each"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token FUNCTION // "function"
%token RETURN   // "return"
%token PRINT    // "print"
%token IN       // "in"
%token END      // "end"
%token PLUS     // '+'
%token MINUS    // '-'
%token AST      // '*'
%token SLASH    // '/'
%token MOD      // "mod"
%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRACKET // '['
%token RBRACKET // ']'
%token LCBRACKET// '{'
%token RCBRACKET// '}'
%token EQ       // '='
%token NE       // "!="
%token AND      // "and"
%token OR       // "or"
%token NOT      // "not"
%token LT       // '<'
%token LE       // "<="
%token GT       // '>'
%token GE       // ">="
%token COMMA    // ","
%token LEFTARROW// "<-"
%token DOT      // '.'
%token NULL     // "null"
 
%token NEWLINE  // '\n'
%token EOF 
            
// 結合力(優先度が低い順)
%left AND OR
%nonassoc NOT
%left EQ LT LE GT GE NE
%left PLUS MINUS
%right LEFTARROW
%left AST
%left DOT
%nonassoc LPAREN

%start main
%type <Syntax.Program.t> main;
%%
// 
main:  
  | dd=nonempty_list(d=fundef; list(NEWLINE) {d}); eof { dd }
;
fundef:  
  | FUNCTION; fname=IDENT;
    LPAREN; prms=separated_list(COMMA,IDENT); RPAREN; NEWLINE;
    ss=suite; NEWLINE;
    END; FUNCTION { FunDef.FD(fname,prms,ss) }
;                                                                                                                     close:
  | NEWLINE { () }
  | INDENT NEWLINE DEDENT { () }
;
eof:
  | list(close); EOF { () }
;                   
exp:
  | LPAREN; e=exp; RPAREN { e }    
  | n=INT    { Exp.Num n }
  | x=IDENT { Exp.Var x }
  | NULL { Exp.Null } 
  | e1=exp; PLUS; e2=exp { Exp.Add(e1,e2) }
;
cond:
  | LPAREN; c=cond; RPAREN { c }
  | TRUE { Cond.True }
  | FALSE { Cond.False }
  | e1=exp; EQ; e2=exp { Cond.Eq(e1,e2) }
  | e1=exp; NE; e2=exp { Cond.Neq(e1,e2) }    
  | e1=exp; LT; e2=exp { Cond.Lt(e1,e2) }
  | e1=exp; GT; e2=exp { Cond.Gt(e1,e2) }
  | e1=exp; LE; e2=exp { Cond.Le(e1,e2) }
  | e1=exp; GE; e2=exp { Cond.Ge(e1,e2) }
  | c1=cond; AND;c2=cond { Cond.And(c1,c2) }
  | c1=cond; OR; c2=cond { Cond.Or(c1,c2) }
  | NOT; c=cond { Cond.Not c }
;
stmt:
  | x=IDENT; LEFTARROW; e=exp { Stmt.Asgn(x,e) }
  | RETURN; eopt=option(e=exp {e}) { Stmt.Return eopt }
  | PRINT; e=exp { Stmt.Print e }
  | fname=IDENT; LPAREN; ee=separated_list(COMMA,e=exp {e}); RPAREN { Stmt.Fcall(fname,ee) }
  | WHILE; c=cond; NEWLINE; ss=suite { Stmt.While(c,ss) }
/// If-expression ## if e : block (else if e: block )* | (else : block)? )
///  | IF; q = patexp; COLON; eeThen = py_suite; nonempty_list(NEWLINE); ELSE; COLON; eeElse = py_suite
///        { packExp @@ P.If (unpackExp q, P.Block eeThen, P.Block eeElse) }
;
suite:
  | INDENT; ss=list(s=stmt; nonempty_list(NEWLINE) {s}); DEDENT { ss }
;
