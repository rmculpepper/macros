#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide define-relation
         relation-add!
         relation-contains?
         relation?)

(struct relation (fields ht updater))

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
        (~optional (~seq #:allow-update? update?:expr)
                   #:defaults ([update? #'#f]))
        (~seq #:function (~var fn (fundep (syntax->list #'(field ...))))) ...
        (~var r (row (syntax->list #'(field ...)))) ...)
     (with-syntax ([fields #'(field ...)])
       #'(begin (define-values (name fn.name ...)
                  (let ([main-ht (make-hash)]
                        [fn.ht (make-hash)] ...)
                    (define (update! row)
                      (let-values ([fields (vector->values row)])
                        (let ([key (vector fn.arg ...)])
                          (unless (eq? none (hash-ref fn.ht key none))
                            (error 'define-relation
                                   "functional dependency ~a violated: duplicate for key ~.s"
                                   '(fn.name : fn.arg ... -> fn.res ...)
                                   (vector->list key)))
                          (hash-set! fn.ht key (vector fn.res ...)))
                        ...
                        (let ([key (vector field ...)])
                          (unless (eq? none (hash-ref main-ht key none))
                            (error 'define-relation
                                   "duplicate entry: ~.s" (vector->list key)))
                          (hash-set! main-ht key key))))
                    (define (fn.name fn.arg ... [default none])
                      (let* ([key (vector fn.arg ...)]
                             [result (hash-ref fn.ht key none)])
                        (if (eq? result none)
                            (cond [(eq? default none)
                                   (error 'fn.name "no entry for key: ~.s" (vector->list key))]
                                  [(procedure? default) (default)]
                                  [else default])
                            (if (pair? '(fn.res ...))
                                (vector->values result)
                                #t))))
                    ...
                    (for ([row (in-list (list (vector r.e ...) ...))])
                      (update! row))
                    (values (relation '(field ...) main-ht (and update? update!))
                            fn.name ...)))))]))

(define (relation-add! rel row)
  (let ([update! (relation-updater rel)])
    (unless update!
      (error 'relation-add! "relation does not allow update: ~e" rel))
    (update! (list->vector row))))

(define (relation-contains? rel row)
  (and (hash-ref (relation-ht rel) (list->vector row) #f) #t))

(define-relation nums (n d)
  #:function (get-d : n -> d)
  (0 "zero")
  (1 "one")
  (2 "two"))
