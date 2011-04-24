#lang racket/base
(require (for-syntax racket/base)
         scribble/manual
         (for-label racket/base
                    racket/contract
                    (planet ryanc/macros:2:0/lazy-require)
                    (planet ryanc/macros:2:0/relation)
                    (planet ryanc/macros:2:0/transformer)))
(provide (all-defined-out)
         (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)
                    (all-from-out (planet ryanc/macros:2:0/lazy-require))
                    (all-from-out (planet ryanc/macros:2:0/relation))
                    (all-from-out (planet ryanc/macros:2:0/transformer))))

(define (my-package-version) "2.0")
(define (my-require-form) (racket (require #,(racketmodname (planet ryanc/macros:2:0)))))

(define-syntax-rule (defmy name underlying)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(name id)
       (identifier? #'id)
       (with-syntax ([mod
                      (datum->syntax #'id
                                     (string->symbol
                                      (format "ryanc/macros:2:0/~a" (syntax-e #'id))))])
         #'(underlying (planet mod)))])))

(defmy my-defmodule defmodule)
(defmy my-defmodule/nd defmodule/nd)
(defmy my-declare-exporting declare-exporting)
(defmy my-racketmodname racketmodname)

(define-syntax-rule (defmodule/nd mod)
  (defmodule*/no-declare (mod)))
