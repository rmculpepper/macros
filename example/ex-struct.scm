(module ex-struct mzscheme
  (require "../struct.ss")
  (require (lib "pretty.ss"))
  (provide (all-defined))

  (define-struct-property prop:foo)
  (define-struct-property prop:bar)

  (define-struct* A (x y z))
  (define-struct* B (q [r (#:immutable)] c)
    [#:procedure (lambda (self) (list (B-q self) (B-r self)))]
    [#:property prop:foo 'baker]
    [#:property prop:bar 'present]
    #:transparent #;#:subst)
  (define-struct* C (a [b (#:auto)])
    [#:property prop:foo 'charlie]
    [#:auto-value 'b]
    #:transparent)
  (define-struct* Bprime ()
    [#:super B]
    #:transparent)

  (define a1 (make-A 'athens 'sparta 'olympia))
  (define b1 (make-B 'three 'four 'five))
  (define c1 (make-C 'a))
  )
