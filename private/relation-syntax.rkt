#lang racket/base
(require syntax/parse
         racket/dict)
(provide (all-defined-out))

(define-syntax-class proper-id
  #:description "identifier other than `:' or `->'"
  (pattern (~and _:id (~not (~datum :)) (~not (~datum ->)))))

(define-syntax-class field-list
  #:description "list of distinct field names"
  (pattern (fld:proper-id ...)
           #:fail-when (check-duplicate (syntax->list #'(fld ...)) #:key syntax-e)
           "duplicate field name"))

(define-syntax-class (field-name names)
  (pattern field:proper-id
           #:when (for/or ([n names]) (bound-identifier=? #'field n))))

(define-syntax-class (fundep names)
  #:description "functional dependency signature"
  (pattern (name (~datum :)
                 arg ...
                 (~datum ->)
                 (~between res 1 +inf.0
                           #:too-few "expected at least one result field name") ...)
           #:declare name proper-id
           #:declare arg (field-name names)
           #:declare res (field-name names))
  (pattern (name (~datum :)
                 arg ...
                 (~datum ->) #t)
           #:declare name proper-id
           #:declare arg (field-name names)
           #:with (res ...) #'()))

(define-syntax-class (row names)
  (pattern (e:expr ...)
           #:fail-unless (= (length (syntax->list #'(e ...))) (length names))
           (format "wrong number of expressions in row (expected ~s)"
                   (length names))))

;; ----

(define (eq-able? x)
  (or (boolean? x)
      (symbol? x)
      (keyword? x)
      (null? x)
      (fixnum? x)))

(define (generate-temporary)
  (car (generate-temporaries '(g))))

;; -- from unstable/list

;; check-duplicate : (listof X)
;;                   #:key (X -> K)
;;                   #:same? (or/c (K K -> bool) dict?)
;;                -> X or #f
(define (check-duplicate items
                        #:key [key values]
                        #:same? [same? equal?])
  (cond [(procedure? same?)
         (cond [(eq? same? equal?)
                (check-duplicate/t items key (make-hash) #t)]
               [(eq? same? eq?)
                (check-duplicate/t items key (make-hasheq) #t)]
               [(eq? same? eqv?)
                (check-duplicate/t items key (make-hasheqv) #t)]
               [else
                (check-duplicate/list items key same?)])]
        [(dict? same?)
         (let ([dict same?])
           (if (dict-mutable? dict)
               (check-duplicate/t items key dict #t)
               (check-duplicate/t items key dict #f)))]))
(define (check-duplicate/t items key table mutating?)
  (let loop ([items items] [table table])
    (and (pair? items)
         (let ([key-item (key (car items))])
           (if (dict-ref table key-item #f)
               (car items)
               (loop (cdr items) (if mutating?
                                     (begin (dict-set! table key-item #t) table)
                                     (dict-set table key-item #t))))))))
(define (check-duplicate/list items key same?)
  (let loop ([items items] [sofar null])
    (and (pair? items)
         (let ([key-item (key (car items))])
           (if (for/or ([prev (in-list sofar)])
                 (same? key-item prev))
               (car items)
               (loop (cdr items) (cons key-item sofar)))))))
