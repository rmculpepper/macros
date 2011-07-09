#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          planet/scribble
          planet/version
          (for-label racket/base
                     racket/contract
                     racket/runtime-path
                     (this-package-in lazy-require)
                     (this-package-in relation)
                     (this-package-in transformer)))

@(define the-eval (make-base-eval))
@(the-eval '(require "lazy-require.rkt" "relation.rkt"
                     (for-syntax racket/base "transformer.rkt")))

@(define the-version
   (format "~a.~a" (this-package-version-maj) (this-package-version-min)))

@title[#:version the-version]{macros: Miscellaneous macros}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@section{Lazy Require}

@defmodule/this-package[lazy-require]

@defform[(define-lazy-require-definer define-id mod-expr)
         #:contracts ([mod-expr module-path?])]{

Defines @racket[define-id] as a definition form for lazily requiring
procedures from the module specified by @racket[mod-expr]. As with
@racket[define-runtime-module-path-index], @racket[mod-expr] is
evaluated both in phase 0 and phase 1.

The syntax of @racket[define-id] is

@specsubform[(define-id id ...)]

A @racket[define-id] form binds each @racket[_id] to a procedure that,
when called, dynamically requires @racket[_id] from the module
specified by @racket[mod-expr] and calls it with the same
arguments. The underlying procedure is cached, so
@racket[dynamic-require] is only called once for each @racket[_id].
}


@section{Relations}

@defmodule/this-package[relation]

@defform/subs[#:literals (:)
              (define-relation rel-id (field-id ...)
                functional-dependency ...
                (field-expr ...) ...)
              ([functional-dependency
                (code:line #:function f-d-sig)]
               [f-d-sig
                (fn-id : dom-field-id ... -> rng-field-id ...+)
                (fn-id : dom-field-id ... -> #t)])]{

Defines @racket[rel-id] as a relation value and each @racket[fn-id] as
a function from the given @racket[dom-field-id]s to the
@racket[rng-field-id]s. Each @racket[(field-expr _...)] is a tuple in
the relation; each tuple must be distinct, or an error is raised. In
addition, the relation must satisfy the given functional dependencies:
a functional dependency is satisfied if for any values of the
@racket[dom-field-id]s there is at most one tuple in the relation with
matching values for those fields. All field comparisons use
@racket[equal?].

Each @racket[fn-id] is bound to a function that accepts either N or
N+1 arguments, where N is the number of that function's
@racket[dom-field-id]s. The final optional argument is used if the
relation has no tuple with the given key; if it is a procedure, it is
applied, otherwise it is returned. If the relation contains a tuple
matching the given key, the @racket[fn-id] function returns the values
of the @racket[rng-field-id] fields as multiple values, or @racket[#t]
if the range is specified as @racket[#t]. If the relation does not
contain a matching tuple, the result is determined by the optional
default argument as described above; if no default argument is given,
the function raises an error.

By default, each of the @racket[fn-id] functions is backed by a hash
table. If, however, a function has a domain of exactly one field and
@racket[define-relation] can statically determine the value of that
field for every tuple in the relation (that is, it is always expressed
as a literal), and if the values are all comparable with @racket[eqv?],
the function is implemented with a @racket[case] expression instead.

If @racket[rel-id] is @racket[_], no relation value is produced, but
the @racket[fn-id]s are still defined.

@examples[#:eval the-eval
(define-relation _ (n s)
  #:function (get-s : n -> s)
  #:function (get-n : s -> n)
  (0 "zero")
  (1 "one")
  (2 "two")
  (3 "three"))
(get-s 0)
(get-s 1 #f)
(get-s 10 #f)

(define-relation _ (n s)
  #:function (get-s : n -> s)
  (0 "zero")
  (0 "nothing") (code:comment "BAD")
  (1 "one")) 
]
}


@section{Transformers}

@defmodule/this-package[transformer]

@defproc[(id->expr-transformer [expr syntax?])
         (-> syntax? syntax?)]{

Produces a transformer that, when bound to an identifier as a macro,
rewrites occurrence of that macro with @racket[expr], whether the
identifier occurs in operator position or as a variable reference.

The resulting transformer preserves the lexical context of the
application form when the identifier occurs in operator position. This
is important for correct behavior in contexts where @racket[#%app] has
been rebound.

@examples[#:eval the-eval
(define counter 0)
(define-syntax add (id->expr-transformer #'+))
(add 1 2)
(define-syntax-rule (bad-add x y)
  (+ x y))
(let-syntax ([#%app
              (syntax-rules ()
                [(_ op arg ...)
                 (let ([result (op arg ...)])
                   (printf "got ~s!\n" result)
                   result)])])
  (add 1 2)
  (bad-add 3 4))
]
}


@(close-eval the-eval)
