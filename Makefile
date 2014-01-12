OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLC=ocamlc
OCAMLOPT=ocamlopt -g
OCAMLDEP=ocamldep

INCLUDES=

OCAMLFLAGS=$(INCLUDES)
OCAMLDEPFLAGS=$(INCLUDES)

BYTEFILES=misc.cmo regexp.cmo DFA.cmo NFA.cmo lexer.cmo conversions.cmo parser.cmo
OPTFILES=misc.cmx regexp.cmx DFA.cmx NFA.cmx lexer.cmx conversions.cmx parser.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
		$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

all: opt

prof: $(OPTFILES)
	$(OCAMLOPT) -p $(INCLUDES) $(OPTFILES) -o prof prof.ml

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
	rm -f prof gmon.out

very_clean:
	rm -f *.cm[aoix] *.o
	rm -f lexer.ml parser.ml parser.mli
	rm -f main
	rm -f prof gmon.out


include .depend
