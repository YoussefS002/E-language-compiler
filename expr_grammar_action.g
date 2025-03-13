tokens SYM_EOF SYM_IDENTIFIER<string> SYM_INTEGER<int> SYM_PLUS SYM_MINUS SYM_ASTERISK SYM_DIV SYM_MOD
tokens SYM_LPARENTHESIS SYM_RPARENTHESIS SYM_LBRACE SYM_RBRACE
tokens SYM_ASSIGN SYM_SEMICOLON SYM_RETURN SYM_IF SYM_WHILE SYM_ELSE SYM_COMMA SYM_PRINT
tokens SYM_EQUALITY SYM_NOTEQ SYM_LT SYM_LEQ SYM_GT SYM_GEQ
non-terminals S INSTR<tree> INSTRS<tree list> LINSTRS ELSE EXPR FACTOR
non-terminals LPARAMS REST_PARAMS
non-terminals IDENTIFIER INTEGER
non-terminals FUNDEF FUNDEFS
non-terminals ADD_EXPRS ADD_EXPR
non-terminals MUL_EXPRS MUL_EXPR
non-terminals CMP_EXPRS CMP_EXPR
non-terminals EQ_EXPRS EQ_EXPR

non-terminals AFTER_IDENTIFIER LARGS REST_ARGS

axiom S
{

  open Symbols
  open Ast
  open BatPrintf
  open BatBuffer
  open Batteries
  open Utils

  type after_id =
  | Assign of tree
  | Funcall of tree list
  | Nothing

  (* TODO *)
  let rec resolve_associativity (term : tree) (other : (tag * tree) list) =
       (* TODO *)
      match List.rev other with
      | [] -> term
      | (high_tag, right_side)::rest -> Node(high_tag, [resolve_associativity term (List.rev rest); right_side])
}

rules
S -> FUNDEFS SYM_EOF { Node(Tlistglobdef, $1) }
FUNDEFS -> FUNDEF FUNDEFS { $1::$2 }
FUNDEFS -> { [] }
FUNDEF -> IDENTIFIER SYM_LPARENTHESIS LPARAMS SYM_RPARENTHESIS INSTR { Node(Tfundef, [Node(Tfunname, [$1]); Node(Tfunargs, $3); Node(Tfunbody, [$5])]) }

LPARAMS -> IDENTIFIER REST_PARAMS { Node(Targ, [$1])::$2 }
LPARAMS -> { [] }
REST_PARAMS -> SYM_COMMA LPARAMS { $2 }
REST_PARAMS -> { [] }

LARGS -> EXPR REST_ARGS { $1::$2 }
LARGS -> { [] }
REST_ARGS -> SYM_COMMA LARGS { $2 }
REST_ARGS -> { [] }

LINSTRS -> INSTR INSTRS { Node(Tblock, $1::$2) }
LINSTRS -> { NullLeaf }
INSTRS -> INSTR INSTRS { $1::$2 }
INSTRS -> { [] }

INSTR -> SYM_IF SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS SYM_LBRACE LINSTRS SYM_RBRACE ELSE { Node(Tif, [$3; $6; $8]) }
INSTR -> SYM_WHILE SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS INSTR { Node(Twhile, [$3; $5]) }
INSTR -> SYM_RETURN EXPR SYM_SEMICOLON { Node(Treturn, [$2]) }
INSTR -> SYM_PRINT SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS SYM_SEMICOLON { Node(Tprint, [$3]) }
INSTR -> IDENTIFIER AFTER_IDENTIFIER SYM_SEMICOLON { 
  match $2 with
  | Assign exp -> Node(Tassign, [$1; exp]) 
  | Funcall args -> Node(Tcall, [$1; Node(Targs, args)]) 
  | _ -> $1 
}
INSTR -> SYM_LBRACE LINSTRS SYM_RBRACE { $2 }

AFTER_IDENTIFIER -> SYM_ASSIGN EXPR { Assign $2 }
AFTER_IDENTIFIER -> SYM_LPARENTHESIS LARGS SYM_RPARENTHESIS { Funcall $2 }
AFTER_IDENTIFIER -> { Nothing }

ELSE -> SYM_ELSE SYM_LBRACE LINSTRS SYM_RBRACE { $3 }
ELSE -> { NullLeaf }

EXPR -> EQ_EXPR EQ_EXPRS { resolve_associativity $1 $2 }
EQ_EXPR -> CMP_EXPR CMP_EXPRS { resolve_associativity $1 $2 }
CMP_EXPR -> ADD_EXPR ADD_EXPRS { resolve_associativity $1 $2 }
ADD_EXPR -> MUL_EXPR MUL_EXPRS { resolve_associativity $1 $2 }
ADD_EXPR -> SYM_MINUS MUL_EXPR MUL_EXPRS { resolve_associativity (Node(Tneg, [$2])) $3 }
ADD_EXPR -> SYM_PLUS MUL_EXPR MUL_EXPRS { resolve_associativity $2 $3 }
MUL_EXPR -> FACTOR { $1 }

EQ_EXPRS -> SYM_EQUALITY EQ_EXPR EQ_EXPRS { (Tceq, $2)::$3 } 
EQ_EXPRS -> SYM_NOTEQ EQ_EXPR EQ_EXPRS { (Tne, $2)::$3 } 
EQ_EXPRS -> { [] } 

CMP_EXPRS -> SYM_LT CMP_EXPR CMP_EXPRS { (Tclt, $2)::$3 } 
CMP_EXPRS -> SYM_LEQ CMP_EXPR CMP_EXPRS { (Tcle, $2)::$3 } 
CMP_EXPRS -> SYM_GT CMP_EXPR CMP_EXPRS { (Tcgt, $2)::$3 }
CMP_EXPRS -> SYM_GEQ CMP_EXPR CMP_EXPRS { (Tcge, $2)::$3 } 
CMP_EXPRS -> { [] } 

ADD_EXPRS -> SYM_PLUS ADD_EXPR ADD_EXPRS { (Tadd, $2)::$3 } 
ADD_EXPRS -> SYM_MINUS ADD_EXPR ADD_EXPRS { (Tsub, $2)::$3 }
ADD_EXPRS -> { [] } 

MUL_EXPRS -> SYM_ASTERISK MUL_EXPR MUL_EXPRS { (Tmul, $2)::$3 }
MUL_EXPRS -> SYM_DIV MUL_EXPR MUL_EXPRS { (Tdiv, $2)::$3 } 
MUL_EXPRS -> SYM_MOD MUL_EXPR MUL_EXPRS { (Tmod, $2)::$3 } 
MUL_EXPRS -> { [] }

FACTOR -> INTEGER { $1 }
FACTOR -> IDENTIFIER AFTER_IDENTIFIER { 
  match $2 with
  | Funcall args -> Node(Tcall, [$1; Node(Targs, args)])
  | Nothing -> $1
  | _ -> $1
}
FACTOR -> SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS { $2 }

IDENTIFIER -> SYM_IDENTIFIER {StringLeaf $1}
INTEGER -> SYM_INTEGER {IntLeaf $1}
