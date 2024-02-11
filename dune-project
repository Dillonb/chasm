(lang dune 3.13)

(name chasm)

(generate_opam_files true)

(source
 (github Dillonb/chasm))

(authors "Dillon Beliveau")

(license MIT)

(package
 (name chasm)
 (synopsis "x64 assembler")
 (description "x64 assembler intended to be used at runtime")
 (depends ocaml dune)
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
