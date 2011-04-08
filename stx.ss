
(module stx mzscheme
  (require (lib "stx.ss" "syntax"))
  (provide (all-from (lib "stx.ss" "syntax"))
           stx-caar
           stx-cadr
           stx-cdar
           stx-cddr

           stx-keyword?
           stx-string?
           stx-bytes?
           stx-vector?
           stx-number?
           stx-exact?
           stx-inexact?
           stx-rational?
           stx-real?
           stx-integer?
           stx-positive-integer?
           stx-nonnegative-integer?
           stx-boolean?
           stx-true?
           stx-false?
           stx-nonfalse?

           check-stx-keyword
           check-stx-string
           check-stx-bytes
           check-stx-vector
           check-stx-number
           check-stx-exact
           check-stx-inexact
           check-stx-rational
           check-stx-real
           check-stx-integer
           check-stx-positive-integer
           check-stx-nonnegative-integer
           check-stx-boolean
           check-stx-true
           check-stx-false
           check-stx-nonfalse

           stx-keyword-value
           stx-string-value
           stx-bytes-value
           stx-number-value
           stx-boolean-value

           symbol-append

           literal-identifier=?
           
           syntax-matches-pattern
           with-syntax*
           )
  
  (define (stx-caar x) (stx-car (stx-car x)))
  (define (stx-cadr x) (stx-car (stx-cdr x)))
  (define (stx-cdar x) (stx-cdr (stx-car x)))
  (define (stx-cddr x) (stx-cdr (stx-cdr x)))

  (define-syntax mk-stx-predicate
    (syntax-rules ()
      [(_ pred ...)
       (lambda (x)
         (let ([y (syntax-e x)])
           (and (syntax? x) (pred y) ...)))]))
  
  (define (nonnegative? x) (or (zero? x) (positive? x)))
  
  (define stx-keyword? (mk-stx-predicate keyword?))
  (define stx-string? (mk-stx-predicate string?))
  (define stx-bytes? (mk-stx-predicate bytes?))
  (define stx-vector? (mk-stx-predicate vector?))
  (define stx-number? (mk-stx-predicate number?))
  (define stx-exact? (mk-stx-predicate number? exact?))
  (define stx-inexact? (mk-stx-predicate number? inexact?))
  (define stx-rational? (mk-stx-predicate rational?))
  (define stx-real? (mk-stx-predicate real?))
  (define stx-integer? (mk-stx-predicate real? exact? integer?))
  (define stx-positive-integer? (mk-stx-predicate real? exact? integer? positive?))
  (define stx-nonnegative-integer? (mk-stx-predicate real? exact? integer? nonnegative?))
  (define stx-boolean? (mk-stx-predicate boolean?))
  (define stx-true? (mk-stx-predicate (lambda (x) (eq? x #t))))
  (define stx-false? (mk-stx-predicate not))
  (define stx-nonfalse? (mk-stx-predicate values))
  
  ;; (stx-list-of predicate stx)
  ;; (stx-vector-of predicate stx)
  ;; ?? (stx-pair-of predicate predicate stx)
  
  
  (define (mk-check predicate message)
    (case-lambda
      [(x)
       (unless (predicate x) (raise-syntax-error #f message x))
       x]
      [(x tag) 
       (unless (predicate x) (raise-syntax-error #f (string-append tag ": " message) x))
       x]))
  
  (define check-stx-keyword (mk-check stx-keyword? "expected keyword"))
  (define check-identifier (mk-check identifier? "expected identifier"))
  (define check-stx-string (mk-check stx-string? "expected string"))
  (define check-stx-bytes (mk-check stx-bytes? "expected bytes"))
  (define check-stx-vector (mk-check stx-vector? "expected vector"))
  (define check-stx-number (mk-check stx-number? "expected number"))
  (define check-stx-exact (mk-check stx-exact? "expected exact number"))
  (define check-stx-inexact (mk-check stx-inexact? "expected inexact number"))
  (define check-stx-rational (mk-check stx-rational? "expected rational number"))
  (define check-stx-real (mk-check stx-real? "expected real number"))
  (define check-stx-integer (mk-check stx-integer? "expected integer"))
  (define check-stx-positive-integer
    (mk-check stx-positive-integer? "expected positive integer"))
  (define check-stx-nonnegative-integer
    (mk-check stx-nonnegative-integer? "expected nonnegative integer"))
  (define check-stx-boolean (mk-check stx-boolean? "expected boolean"))
  (define check-stx-true (mk-check stx-true? "expected true literal"))
  (define check-stx-false (mk-check stx-false? "expected false literal"))
  (define check-stx-nonfalse (mk-check stx-nonfalse? "expected non-false"))

  (define-syntax define-stx-value-function
    (syntax-rules ()
      [(mk-stx-value name typename predicate)
       (define (name x)
         (unless (predicate x)
           (raise-type-error 'name (string-append "syntax " typename) x))
         (syntax-e x))]))
  
  (define-stx-value-function stx-keyword-value "keyword" stx-keyword?)
  (define-stx-value-function stx-string-value "string" stx-string?)
  (define-stx-value-function stx-bytes-value "bytes" stx-bytes?)
  (define-stx-value-function stx-number-value "number" stx-number?)
  (define-stx-value-function stx-boolean-value "boolean" stx-boolean?)
  
  (define-values (stx-map stx-andmap stx-ormap)
    (let ([mk-stx-map-like
           (lambda (functional)
             (case-lambda
               [(f stxs) (functional f (stx->list stxs))]
               [(f . stxss) (apply functional f (map stx->list stxss))]))])
      (values (mk-stx-map-like map)
              (mk-stx-map-like andmap)
              (mk-stx-map-like ormap))))
  
  (define (symbol-append . items)
    (define (->string x)
      (cond [(string? x) x]
            [(keyword? x) (keyword->string x)]
            [(symbol? x) (symbol->string x)]
            [(identifier? x) (symbol->string (syntax-e x))]
            [else (raise-type-error 'symbol-append
                                    "string, symbol, keyword, or identifier" x)]))
    (string->symbol (apply string-append (map ->string items))))
  
  (define (literal-identifier=? a b)
    (cond [(and (symbol? a) (symbol? b))
           (eq? a b)]
          [(identifier? a)
           (literal-identifier=? (syntax-e a) b)]
          [(identifier? b)
           (literal-identifier=? a (syntax-e b))]))
  
  (define-syntax syntax-matches-pattern
    (syntax-rules ()
      [(syntax-matches-pattern stx kws pattern ...)
       (syntax-case stx kws
         [pattern #t] ...
         [_ #f])]))

  (define-syntax with-syntax*
    (syntax-rules ()
      [(with-syntax* () . body)
       (let () . body)]
      [(with-syntax* ([lhs0 rhs0] . bindings) . body)
       (with-syntax ([lhs0 rhs0])
         (with-syntax* bindings . body))]))

  )
