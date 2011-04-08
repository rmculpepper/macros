#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide define-relation
         relation?)

(struct relation (fields ht))

(define none (gensym 'none))

(define-syntax (define-relation stx)

  (define-syntax-class (field-name names)
    (pattern field:id
             #:when (for/or ([n names]) (bound-identifier=? #'field n))))

  (define-syntax-class (fundep names)
    #:description "functional dependency signature"
    (pattern (name:id (~datum :) (~and (~not (~datum ->)) arg) ...
                      (~datum ->) res ...)
             #:declare arg (field-name names)
             #:declare res (field-name names)
             #:with (ht) (generate-temporaries #'(name))))

  (define-syntax-class (row names)
    (pattern (e:expr ...)
             #:fail-unless (= (length (syntax->list #'(e ...))) (length names))
                           (format "wrong number of expressions in row (expected ~s)"
                                   (length names))))

  (syntax-parse stx
    [(_ name:id
        (~describe "field names" (field ...))
        (~seq #:function (~var fn (fundep (syntax->list #'(field ...))))) ...
        (~var r (row (syntax->list #'(field ...)))) ...)
     (with-syntax ([fields #'(field ...)])
       #'(begin (define-values (main-ht fn.ht ...)
                  (let ([main-ht (make-hash)]
                        [fn.ht (make-hash)] ...)
                    (for ([row (in-list (list (vector r.e ...) ...))])
                      (let-values ([fields (vector->values row)])
                        (let ([key (vector fn.arg ...)])
                          (unless (eq? none (hash-ref fn.ht key none))
                            (error 'define-relation
                                   "functional dependency ~a violated: duplicate for key ~s"
                                   '(fn.name : fn.arg ... -> fn.res ...)
                                   (list 'fn.arg ...)))
                          (hash-set! fn.ht key (vector fn.res ...)))))
                    ...
                    (values main-ht fn.ht ...)))
                (define (fn.name fn.arg ... [default none])
                  (let* ([key (vector fn.arg ...)]
                         [result (hash-ref fn.ht key none)])
                    (if (eq? result none)
                        (cond [(eq? default none)
                               (error 'fn.name "no entry for key: ~s" (vector->list key))]
                              [(procedure? default) (default)]
                              [else default])
                        (if (pair? '(fn.res ...))
                            (vector->values result)
                            #t))))
                ...
                (define name (relation '(field ...) main-ht))))]))

(define-relation nums (n d)
  #:function (get-d : n -> d)
  (0 "zero")
  (1 "one")
  (2 "two"))
