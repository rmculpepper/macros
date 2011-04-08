
(module structlike mzscheme
  (require-for-syntax "../stx.ss")
  (provide define-struct-like)

  ;; define-struct-like SYNTAX
  (define-syntax (define-struct-like stx)
    (syntax-case stx ()
      [(define-struct-like name (field ...) expr)
       (with-syntax ([struct-like-name (symbol->string (syntax-e #'name))]
                     [make-X
                      (datum->syntax-object #'name (symbol-append 'make- #'name))]
                     [register-as-X
                      (datum->syntax-object #'name (symbol-append 'register-as- #'name))]
                     [X? (datum->syntax-object #'name (symbol-append #'name '?))]
                     [struct:X
                      (datum->syntax-object #'name (symbol-append 'struct: #'name))]
                     [(field-n ...)
                      (let loop ([fields (syntax->list #'(field ...))] [i 0])
                        (if (null? fields) null (cons i (loop (cdr fields) (add1 i)))))]
                     [(field-ref ...)
                      (map (lambda (f)
                             (datum->syntax-object #'name (symbol-append #'name '- f)))
                           (syntax->list #'(field ...)))]
                     [(field-set ...)
                      (map (lambda (f)
                             (datum->syntax-object
                              #'name
                              (symbol-append 'set- #'name '- f '!)))
                           (syntax->list #'(field ...)))])
         #'(begin
             (define struct:X #f)
             (define-syntax name #f)
             (define-values (make-X register-as-X X? field-ref ... field-set ...)
               (let ()
                 (define the-hash-table (make-hash-table 'weak))
                 (define (make-X field ...)
                   (let ([newval expr])
                     (register-as-X newval field ...)
                     newval))
                 (define (register-as-X value field ...)
                   (when (hash-table-get the-hash-table value #f)
                     (error 'register-as-X
                            "struct-like value must be freshly allocated: ~e"
                            value))
                   (hash-table-put! the-hash-table value (vector field ...)))
                 (define (get-record blame x)
                   (let ([record (hash-table-get the-hash-table x #f)])
                     (unless record
                       (raise-type-error blame 'struct-like-name x))
                     record))
                 (define (X? x)
                   (and (hash-table-get the-hash-table x #f) #t))
                 (define (field-ref x)
                   (vector-ref (get-record 'field-ref x) field-n))
                 ...
                 (define (field-set x v)
                   (vector-set! (get-record 'field-set x) field-n v))
                 ...
                 (values make-X
                         register-as-X
                         X?
                         field-ref ...
                         field-set ...)))))]))
  )
