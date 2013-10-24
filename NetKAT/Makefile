# NetKAT makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment the following 2 lines for Eclipse/Mac OS X
export SHELL = /bin/bash
export PATH = /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

SOURCES = util.ml ast.ml Parser/parser.mli Parser/lexer.ml Parser/parser.ml Parser/parse.ml repl.ml

all: netkat

clean:
	rm -f netkat
	for X in . Parser; do \
      for Y in cmo cmi output log aux; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

netkat: $(SOURCES)
	ocamlc -o netkat -g -I Parser str.cma $(SOURCES)

Parser/parser.mli Parser/parser.ml: Parser/parser.mly
	ocamlyacc -v Parser/parser.mly

Parser/lexer.ml: Parser/lexer.mll Parser/parser.ml
	ocamllex Parser/lexer.mll