all: ecomp

include opts.mk

.PHONY: ecomp

src/config.ml: configure opts.mk
	./configure ${CONF_OPTS}

.PHONY: alpaga
alpaga/alpaga:
	make -C alpaga

src/generated_parser.ml: expr_grammar_action.g alpaga/alpaga
	./alpaga/alpaga \
			-g expr_grammar_action.g \
			-pml src/generated_parser.ml \
			-t grammar.html

ecomp: src/generated_parser.ml src/config.ml
	make -C src
	ln -sf src/_build/default/main.exe ecomp

clean:
	make -C alpaga clean
	rm -f src/generated_parser.ml
	rm -f grammar.html
	make -C src clean
	rm -f ecomp
	make -C tests clean

test: ecomp
	make -C tests
