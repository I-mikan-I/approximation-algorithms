(lang dune 3.10)

(name approx)

(generate_opam_files true)

(source
 (github I-mikan-I/approximation-algorithms))

(authors "I-mikan-I")

(maintainers "I-mikan-I")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name approx)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  base
  stdio
  ppx_jane
  (ppx_inline_test :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
