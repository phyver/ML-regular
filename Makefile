OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep

INCLUDES=

OCAMLFLAGS=$(INCLUDES)
OCAMLDEPFLAGS=$(INCLUDES)

BYTEFILES=misc.cmo regexp.cmo dfa.cmo lexer.cmo parser.cmo commands.cmo
OPTFILES=misc.cmx regexp.cmx dfa.cmx lexer.cmx parser.cmx commands.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
		$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

all: opt

byte: $(BYTEFILES)
	$(OCAMLC) $(INCLUDES) $(BYTEFILES) -o main main.ml

opt: $(OPTFILES)
	$(OCAMLOPT) $(INCLUDES) $(OPTFILES) -o main main.ml

depend: parser.ml lexer.ml
	$(OCAMLDEP) $(OCAMLDEPFLAGS) *.ml *.mli > .depend

parser.ml:
	$(OCAMLYACC) $(OCAMLYACCFLAGS) parser.mly

lexer.ml:
	$(OCAMLLEX) $(OCAMLLEXFLAGS) lexer.mll

clean:
	rm -f *.cm[aoix] *.o
	rm -f main

very_clean:
	rm -f *.mli
	rm -f *.cm[aoix] *.o
	rm -f lexer.ml parser.ml parser.mli
	rm -f main


include .depend
