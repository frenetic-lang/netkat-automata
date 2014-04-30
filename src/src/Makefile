# NetKAT makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment the following 2 lines for Eclipse/Mac OS X
export SHELL = /bin/bash
export PATH = /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

SOURCES = Util.mli Util.ml Ast.mli Ast.ml Dot.ml Parser.mli Lexer.ml Parser.ml Normalize.mli Normalize.ml Base.mli Base.ml Spines.mli Spines.ml Deriv.mli Deriv.ml Test.mli Test.ml Repl.mli Repl.ml

all: netkat

prof: netkat-prof

clean:
	rm -f netkat
	for X in . Test Parser; do \
      for Y in cmo cmi output log aux; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

netkat: $(SOURCES)
	ocamlopt -o netkat -g -p -I Test -I Parser  $(SOURCES)

netkat-prof: $(SOURCES)
	ocamlcp -o netkat-prof -g -I Test -I Parser -I Dot $(SOURCES)

Parser.mli Parser.ml: Parser.mly
	ocamlyacc -v Parser.mly

Lexer.ml: Lexer.mll Parser.ml
	ocamllex Lexer.mll
