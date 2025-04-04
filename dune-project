(lang dune 3.17)

(name ann)

(generate_opam_files true)

(source
 (github noahdiewald/ann))

(authors "Noah Diewald <github.s0r40@passmail.net>")

(license GPLv3)

(documentation https://github.com/noahdiewald/ann)

(package
 (name ann)
 (synopsis "Annotate text")
 (description "An application for annotating text with a linguistics focus.")
 (depends ocaml dream crunch tyxml core core_unix)
 (tags
  ("annotation" "linguistics" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
