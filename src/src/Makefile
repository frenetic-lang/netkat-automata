# NetKAT makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment the following 2 lines for Eclipse/Mac OS X
export SHELL = /bin/bash
export PATH = /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

SOURCES = Decide_Util.mli Decide_Util.ml Decide_Set.mli Decide_Set.ml Decide_Base.mli Decide_Base.ml Decide_Ast.mli Decide_Ast.ml Decide_Parser.mli Decide_Lexer.ml Decide_Parser.ml Decide_Deriv.mli Decide_Deriv.ml Decide_Dot.ml Decide_Bisimulation.mli Decide_Bisimulation.ml Decide_Loopfree.mli Decide_Loopfree.ml


all: netkat

prof: netkat-prof

test: good-test-dexter

clean:
	rm -f netkat
	for X in . Test Decide_Parser; do \
      for Y in cmo cmi output log aux cmx o; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

netkat: $(SOURCES)
	ocamlopt -p -g -o netkat $(SOURCES) Decide_Repl.mli Decide_Repl.ml

netkat-prof: $(SOURCES)
	ocamlcp -o netkat-prof -g -I Test -I Decide_Parser -I Dot $(SOURCES) DexterTest.ml SingleTest.mla


Decide_Parser.mli Decide_Parser.ml: Decide_Parser.mly
	ocamlyacc -v Decide_Parser.mly

Decide_Lexer.ml: Decide_Lexer.mll Decide_Parser.ml
	ocamllex Decide_Lexer.mll
