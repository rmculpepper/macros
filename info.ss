(module info (lib "infotab.ss" "setup")
  (define name "macros")
  (define blurb
    '("Defines macros for struct definitions and interface-checked class programming. "
      "Also provides utilities for macro writers."))
  (define primary-file (list "struct.ss" "class-iop.ss" "stx.ss" "qd.ss"))
  (define version "1.3")
  (define doc.txt "doc.txt")
  (define categories '(metaprogramming))
  (define can-be-loaded-with 'all)
  (define compile-omit-paths '("example" "experimental")))
