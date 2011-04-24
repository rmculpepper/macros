;; Copyright 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.

#lang setup/infotab
(define name "macros")
(define compile-omit-paths '())
(define blurb
  '("Defines macros for struct definitions and interface-checked class programming. "
    "Also provides utilities for macro writers."))
(define scribblings '(("scribblings/macros.scrbl")))
(define categories '(metaprogramming))
(define can-be-loaded-with 'all)
(define required-core-version "5.1")
