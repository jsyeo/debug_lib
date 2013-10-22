OCAML=ocaml

all: configure build install

configure:
	$(OCAML) setup.ml -configure

build: configure
	$(OCAML) setup.ml -build

install: build
	$(OCAML) setup.ml -install

uninstall:
	$(OCAML) setup.ml -uninstall

clean:
	rm -rf _build
	rm -f setup.data
	rm -f setup.log
