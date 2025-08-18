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
%token DO       // "do"
%token FOR      // "for"
%token EACH     // "each"
%token FROM     // "from"
%token TO       // "to"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token ELSEIF   // "else if"
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
%left PLUS MINUS
%left AST SLASH MOD            
%left DOT
%nonassoc NOT
%nonassoc UNARY

%start main
%type <Syntax.Program.t> main;
%%
// 
main:  
  | dd=nonempty_list(d=fundef; list(NEWLINE) {d}); eof { dd }
;
fundef:  
  | FUNCTION; fname=IDENT;
    LPAREN; prms=separated_list(COMMA,v=IDENT {v}); RPAREN; NEWLINE;
    ss=suite; NEWLINE;
    END; FUNCTION { FunDef.FD(fname,prms,ss) }
    ;
close:
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
  | e1=exp; PLUS;  e2=exp { Exp.Add(e1,e2) } // e1+e2
  | e1=exp; MINUS; e2=exp { Exp.Sub(e1,e2) } // e1-e2
  | e1=exp; AST;   e2=exp { Exp.Mul(e1,e2) } // e1*e2
  | e1=exp; SLASH; e2=exp { Exp.Div(e1,e2) } // e1/e2
  | e1=exp; MOD;   e2=exp { Exp.Mod(e1,e2) } // e1 mod e2
  | MINUS; e=exp; %prec UNARY { Exp.Minus e } // -e
  | e=exp; DOT; fld=IDENT { Exp.Mem(e,fld) } // e.fld
  | e1=exp; LBRACKET; e2=exp; RBRACKET { Exp.Idx(e1,e2) } // e1[e2]
  | fname=IDENT; LPAREN; ee=separated_list(COMMA,e=exp {e}); RPAREN { Exp.Fun(fname,ee) } // f(e1,..,en)
  | LCBRACKET; feL=separated_list(COMMA,f=IDENT;EQ;e=exp {(f,e)}); RCBRACKET { Exp.Str feL } // {fld1=e1,..,fldn=en}
  | LBRACKET; ee=separated_list(COMMA,e=exp {e}); RBRACKET { Exp.Lst ee } // [e1,..,en]
  | TRUE { Exp.True } 
  | FALSE { Exp.False }
  | e1=exp; EQ; e2=exp { Exp.Eq(e1,e2) } // e1=e2
  | e1=exp; NE; e2=exp { Exp.Neq(e1,e2) } // e1!=e2
  | e1=exp; LT; e2=exp { Exp.Lt(e1,e2) } // e1<e2
  | e1=exp; GT; e2=exp { Exp.Gt(e1,e2) } // e1>e2
  | e1=exp; LE; e2=exp { Exp.Le(e1,e2) } // e1<=e2
  | e1=exp; GE; e2=exp { Exp.Ge(e1,e2) } // e1>=e2
  | e1=exp; AND;e2=exp { Exp.And(e1,e2) }  // e1 and e2
  | e1=exp; OR; e2=exp { Exp.Or(e1,e2) } // e1 or e2
  | NOT; e=exp { Exp.Not e } // not e
;
exp_lvar:
  | x=IDENT { Exp.Var x }
  | e=exp_lvar; DOT; fld=IDENT { Exp.Mem(e,fld) } // e.fld
  | e1=exp_lvar; LBRACKET; e2=exp; RBRACKET { Exp.Idx(e1,e2) } // e1[e2]
;
stmt:
  // built-in functions  
  | RETURN; eOpt=option(e=exp {e}) { Stmt.Return eOpt } // return e / return
  | PRINT; e=exp { Stmt.Print e } // print e
    
  // e1<-e2
  | e1=exp_lvar; LEFTARROW; e2=exp { Stmt.Asgn(e1,e2) }
    
  // f(e1,..,en)
  | fname=IDENT; LPAREN; ee=separated_list(COMMA,e=exp {e}); RPAREN { Stmt.Fcall(fname,ee) } 

  // while e do <suite> end while
  | WHILE; e=exp; DO; NEWLINE; ss=suite; NEWLINE; END; WHILE { Stmt.While(e,ss) } 

  // for each i in e <suite> end for
  | FOR; EACH; v=IDENT; IN; e=exp; NEWLINE; ss=suite; NEWLINE; END; FOR { Stmt.Foreach(v,e,ss) }

  // for i from e to e <suite> end for
  | FOR; v=IDENT; FROM; e1=exp; TO; e2=exp; NEWLINE; ss=suite; NEWLINE; END; FOR { Stmt.For(v,e1,e2,ss) }
    
   // if e then <suite> (else if e then <suite>)* [else <suite>] end if
  | IF; e=exp; THEN; NEWLINE; ss=suite; NEWLINE; uuElseIf=list(ELSEIF; e=exp; THEN; NEWLINE; ss=suite; NEWLINE {(e,ss)}); oElse=option(ELSE; NEWLINE; ss=suite; NEWLINE {ss}); END; IF { Stmt.If(e,ss,uuElseIf,oElse) }
;
suite:
  | INDENT; list(NEWLINE); ss=list(s=stmt; nonempty_list(NEWLINE) {s}); DEDENT { ss }
;


