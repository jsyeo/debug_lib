# OCaml Debug Library

OCaml debugging tools

## Requirements

- `ocamlfind`
- `ocamlbuild`

## Installation

Use make.

    make configure
    make build
    make install # installs to SYSTEM_DIR/lib/debuglib

## Usage

When compiling your project with debuglib, you are required to specify
the usage of the `str.cma` library because of `debuglib`'s
dependencies on it.

Say the file that you are compiling is called `cool_prog.ml`, use
`ocamlbuild` with these options.

    ocamlbuild -pkg debuglib -lib str cool_prog.native
