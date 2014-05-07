# NetKAT makefile
#
# targets are:
#
# all -- rebuild the project (default)
# clean -- remove all objects and executables

# uncomment the following 2 lines for Eclipse/Mac OS X
export SHELL = /bin/bash
export PATH = /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

SOURCES = Decide_Util.mli Decide_Util.ml Decide_Ast.mli Decide_Ast.ml Decide_Dot.ml Parser.mli Lexer.ml Parser.ml Decide_Normalize.mli Decide_Normalize.ml Decide_Base.mli Decide_Base.ml Decide_Spines.mli Decide_Spines.ml Decide_Deriv.mli Decide_Deriv.ml Decide_Bisimulation.mli Decide_Bisimulation.ml Decide_Test.mli Decide_Test.ml Decide_Repl.mli Decide_Repl.ml

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
