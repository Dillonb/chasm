(lang dune 3.13)

(name chasm)

(generate_opam_files true)

(using menhir 3.0)

(source
 (github Dillonb/chasm))

(authors "Dillon Beliveau")

(license MIT)

(package
 (name chasm)
 (synopsis "x64 assembler")
 (description "x64 assembler intended to be used at runtime")
 (depends ocaml dune stdint))

(package
 (name chasm_ppx)
 (synopsis "x64 assembler ppx")
 (description "x64 assembler intended to be used at runtime (ppx)")
 (depends chasm ocaml dune stdint menhir ocamlyacc))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
