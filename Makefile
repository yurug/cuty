.PHONY: all clean

JSOFOCAML=js_of_ocaml

SOURCES=$(wildcard *.ml)
OBJECTS=$(SOURCES:.ml=.cmo)

all: cuty.js

cuty.js: $(SOURCES)
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" cuty.js

clean:
	rm cuty.js $(OBJECTS)
