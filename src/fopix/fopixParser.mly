%{

  open FopixAST

%}

%token VAL DEF IN LET IF THEN ELSE EVAL UPPERSAND QMARK NEW
%token PLUS MINUS STAR SLASH GT GTE LT LTE EQUAL PERCENT
%token LPAREN RPAREN LBRACKET RBRACKET ASSIGNS COMMA SEMICOLON EOF
%token<int> INT
%token<string> ID

%nonassoc ELSE IN
%right SEMICOLON
%nonassoc ASSIGNS
%nonassoc GT GTE LT LTE EQUAL
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc LBRACKET

%start<FopixAST.t> program

%%

program: ds=definition* EOF
{
  ds
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}

definition:
  VAL x=ID EQUAL e=expr                          { DefVal (x, e) }
| EVAL e=expr                                    { DefVal ("_", e) }
| DEF f=ID LPAREN xs=separated_list(COMMA, ID) RPAREN EQUAL e=expr
                                                 { DefFun (f, xs, e) }

expr:
  x=INT                                          { Num x }
| UPPERSAND f=ID                                 { FunName f }
| x=ID                                           { Var x }
| LET x=ID EQUAL e1=expr IN e2=expr              { Let (x, e1, e2) }
| IF c=expr THEN t=expr ELSE f=expr              { IfThenElse (c, t, f) }
| l=expr b=binop r=expr                          { BinOp (b, l, r) }
| e=expr LBRACKET i=expr RBRACKET                { BlockGet (e, i) }
| e=expr LBRACKET i=expr RBRACKET ASSIGNS v=expr { BlockSet (e, i, v) }
| NEW LBRACKET e=expr RBRACKET                   { BlockNew e }
| e1=expr SEMICOLON e2=expr                      { Let ("_", e1, e2) }
| LPAREN e=expr RPAREN                           { e }
| QMARK LPAREN e=expr RPAREN LPAREN es=separated_list(COMMA, expr) RPAREN
                                                 { FunCall (e, es) }
| f=ID LPAREN es=separated_list(COMMA, expr) RPAREN
                                                 { FunCall (FunName f, es) }

%inline binop:
  PLUS  { Add }
| MINUS { Sub }
| STAR  { Mul }
| SLASH { Div }
| PERCENT { Mod }
| GT    { Gt }
| GTE   { Ge }
| LT    { Lt }
| LTE   { Le }
| EQUAL { Eq }

