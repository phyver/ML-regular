INSTALLDIR=$(HOME)/.local/bin

OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLC=ocamlc
OCAMLOPT=ocamlopt -g
OCAMLDEP=ocamldep

INCLUDES=

OCAMLFLAGS=$(INCLUDES)
OCAMLDEPFLAGS=$(INCLUDES)

BYTEFILES=common.cmo regexp.cmo DFA.cmo NFA.cmo lexer.cmo conversions.cmo parser.cmo
OPTFILES=common.cmx regexp.cmx DFA.cmx NFA.cmx lexer.cmx conversions.cmx parser.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c -annot $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
		$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

all: opt

prof: $(OPTFILES)
	$(OCAMLOPT) -p $(INCLUDES) $(OPTFILES) -o prof prof.ml

annot: $(BYTEFILES)

byte: $(BYTEFILES)
	$(OCAMLC) $(INCLUDES) $(BYTEFILES) -o mlr main.ml

opt: $(OPTFILES)
	$(OCAMLOPT) $(INCLUDES) $(OPTFILES) -o mlr main.ml

depend: parser.ml lexer.ml
	$(OCAMLDEP) $(OCAMLDEPFLAGS) *.ml *.mli > .depend

parser.ml:
	$(OCAMLYACC) $(OCAMLYACCFLAGS) parser.mly

lexer.ml:
	$(OCAMLLEX) $(OCAMLLEXFLAGS) lexer.mll

install: opt
	install -d $(INSTALLDIR)
	install ./mlr $(INSTALLDIR)

clean:
	rm -f *.cm[aoix] *.o
	rm -f lexer.ml parser.ml parser.mli
	rm -f gmon.out

very_clean:
	rm -f *.cm[aoix] *.o
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.annot
	rm -f ML-regular.tar.gz
	rm -f gmon.out
	rm -f prof mlr


include .depend
