(module struct-props mzscheme
  (require-for-syntax "../stx.ss")
  (provide define-struct-property)
  
  ;; syntax define-struct-property
  ;; Example:
  ;;   (define-struct-property prop:fooable)
  (define-syntax (define-struct-property stx)
    (syntax-case stx ()
      [(_ name . optional-guard)
       (and (identifier? #'name)
            (syntax-matches-pattern #'optional-guard () (g) ()))
       (with-syntax ([predicate
                      (datum->syntax-object #'name (symbol-append #'name '?))]
                     [accessor
                      (datum->syntax-object #'name (symbol-append #'name '-value))])
         #'(define-values (name predicate accessor)
             (make-struct-type-property 'name . optional-guard)))]))

  )