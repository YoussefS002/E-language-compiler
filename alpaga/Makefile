#OCB=ocamlbuild -use-ocamlfind -cflag -g

SRC=ml_parser_generator.ml \
	grammar_parser.ml \
	ll_parser.ml \
	list_utils.ml	\
	grammar.ml \
	grammar_lexer.mll \
	grammar_parser_yacc.mly

TG = alpaga

all: $(TG)

$(TG): $(SRC)
	dune build ml_parser_generator.exe
	ln -sf _build/default/ml_parser_generator.exe alpaga

clean:
	rm -f ml_parser_generator.native $(TG)
	rm -rf _build
