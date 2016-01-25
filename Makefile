INSTALLDIR=$(HOME)/.local/bin
OCAMLBUILD=ocamlbuild

all: native

tags:
	ctags *.ml

native:
	$(OCAMLBUILD) main.native
	@ln -sf ./main.native ./mlr

byte:
	$(OCAMLBUILD) main.byte
	@ln -sf ./main.byte ./mlr

clean:
	$(OCAMLBUILD) -clean
	rm -rf _build
	rm -f main.native main.byte
	rm -f mlr
	rm -f tags
	rm -f gmon.out a.out



