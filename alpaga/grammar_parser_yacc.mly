%{

    open Grammar

    %}

%token EOF EOL TOK NT RULES ARROW AXIOM //LT GT
%token<string> IDENTIFIER
%token<string> CODE
%token<string> TTYPE

%start main
%type <Grammar.grammar> main

%%

  main:
    | AXIOM IDENTIFIER EOL main { let r = $4 in {r with axiom = Some $2 }}
    | TOK list_tokens EOL main { let r = $4 in {r with tokens = r.tokens @ $2} }
    | NT list_nts EOL main { let r = $4 in {r with nonterms = r.nonterms @ $2} }
    | CODE main { let r = $2 in { r with mlcode = Some ($1) }}
    | RULES EOL rules EOF { { tokens = []; nonterms = []; axiom = None;
                              rules = $3; mlcode = None } }
    | EOL main { $2 }
    ;

      typed_tokens:
        | IDENTIFIER TTYPE { ($1, Some $2) }
        | IDENTIFIER { ($1, None)}
    ;


      list_tokens:
        | typed_tokens list_tokens { $1 :: $2}
        | { [] }
    ;


      typed_nts:
        | IDENTIFIER TTYPE { ($1, Some $2) }
        | IDENTIFIER { ($1, None)}
    ;


      list_nts:
        | typed_nts list_nts { $1 :: $2}
        | { [] }
    ;




  list_ident:
    | IDENTIFIER list_ident { $1 :: $2}
    | { [] }
    ;

  rules:
    | rule rules { $1 :: $2 }
    | EOL rules {$2}
    | {[]}
    ;

  rule:
    | IDENTIFIER ARROW list_ident EOL {
          { rule_nt = $1; rule_prods = $3; rule_action = None } }
    | IDENTIFIER ARROW list_ident EOL? CODE EOL {
          { rule_nt = $1; rule_prods = $3; rule_action = Some $5 } }
    ;

%%
