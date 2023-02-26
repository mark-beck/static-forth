(lang dune 3.3)

(name forth)

(generate_opam_files true)

(source
 (github mark-beck/static-forth))

(authors "Mark Beck")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name forth)
 (synopsis "A wip forth interpreter with stack based type checking")
 (description "A wip forth interpreter with stack based type checking")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
