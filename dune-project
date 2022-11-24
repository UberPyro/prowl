(lang dune 3.3)
(using menhir 2.1)

(name prowl)

(generate_opam_files true)

(source
 (github UberPyro/prowl))

(authors "UberPyro")

(maintainers "UberPyro")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name prowl)
 (synopsis "Typed Biconcatenative Language")
 (description "A costack-stack-based programming language inspired by Kitten and Vinegar")
 (depends ocaml dune)
 (tags
  (stack-based functional)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
