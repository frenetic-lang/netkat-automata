# NetKAT makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment the following 2 lines for Eclipse/Mac OS X
export SHELL = /bin/bash
export PATH = /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

SOURCES = Decide_Util.mli Decide_Util.ml Decide_Ast.mli Decide_Ast.ml Parser.mli Lexer.ml Parser.ml Decide_Base.mli Decide_Base.ml Decide_Ast_Cache.mli Decide_Ast_Cache.ml Decide_Spines.mli Decide_Spines.ml Decide_Deriv.mli Decide_Deriv.ml Decide_Dot.ml Decide_Bisimulation.mli Decide_Bisimulation.ml

all: netkat

prof: netkat-prof

test: good-test-dexter

stack-overflow : t26

clean:
	rm -f netkat
	for X in . Test Parser; do \
      for Y in cmo cmi output log aux; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

netkat: $(SOURCES)
	/home/research/.opam/system/bin/ocamlfind ocamlopt -linkpkg -thread -package batteries -o netkat $(SOURCES) Decide_Repl.mli Decide_Repl.ml

netkat-prof: $(SOURCES)
	ocamlfind ocamlcp -linkpkg -thread -package batteries -o netkat-prof -g -I Test -I Parser -I Dot $(SOURCES) DexterTest.ml SingleTest.ml

good-test-dexter: $(SOURCES)
	ocamlopt -o dexter-test -I Test -I Parser $(SOURCES) DexterTest.ml SingleTest.ml

t26: $(SOURCES)
	ocamlopt -o t26 -I Test -I Parser $(SOURCES) Tree_2_6.ml SingleTest26.ml



Parser.mli Parser.ml: Parser.mly
	ocamlyacc -v Parser.mly

Lexer.ml: Lexer.mll Parser.ml
	ocamllex Lexer.mll
