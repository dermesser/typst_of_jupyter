(lang dune 3.9)

(name typst_of_jupyter)
(version 0.1)

(generate_opam_files true)

(authors "Lewin Bormann")

(maintainers "Lewin Bormann")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name typst_of_jupyter)
 (synopsis "Convert jupyter notebooks to typst source code")
 (depends
   ocaml
   ansi-parse
   base
   base64
   core
   core_unix
   dune
   imagelib
   omd
   parsexp
   yojson
   )
 (tags
  (jupyter typst convert)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
