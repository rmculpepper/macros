
(module struct-copy mzscheme
  (require-for-syntax "struct-ct.ss"
                      (lib "list.ss")
                      "../stx.ss")
  (provide make
           #;copy
           #;copy*)
  
  (define-for-syntax (resolve-field/struct info field thunk)
    (let loop ([n 0] [fields (map info:field-field (info:struct-field-infos info))])
      (if (pair? fields)
          (if (eq? (car fields) (syntax-e field))
              (list (+ n (info:struct-total-field-count info))
                    (list (info:struct-name info)
                          field))
              (loop (add1 n) (cdr fields)))
          (thunk))))
  
  ;; resolve-field : info identifier identifier -> sexpr: (number (identifier identifier))
  (define-for-syntax (resolve-field info stype field)
    (cond [(eq? info #t)
           (raise-syntax-error #f 
                               (format "field not found: ~s" (syntax-e field))
                               field)]
          [(stx-false? stype)
           (resolve-field/struct
            info field
            (lambda () (resolve-field (info:struct-super info) stype field)))]
          [(module-identifier=? stype (info:struct-name info))
           (resolve-field/struct 
            info field
            (lambda ()
              (raise-syntax-error #f
                                  (format "field not found: ~s" (syntax-e field))
                                  field)))]
          [else
           (resolve-field (info:struct-super info) stype field)]))
  
  (define-syntax (make stx)
    (syntax-case stx ()
      [(_ type (field-spec value) ...)
       (let ()
         (unless (identifier? #'type)
           (raise-syntax-error #f "expected struct type name" #'type))
         (let* ([static-info
                 (static-struct-info 
                  #'type
                  (lambda ()
                    (raise-syntax-error #f "expected struct type name" #'type)))]
                [total-field-count (info:struct-total-field-count static-info)]
                [field-specs
                 (map (lambda (field-spec)
                        (syntax-case field-spec ()
                          [field
                           (identifier? #'field)
                           #`(#f field)]
                          [(struct-type field)
                           (and (identifier? #'struct-type)
                                (identifier? #'field))
                           field-spec]
                          [else
                           (raise-syntax-error 
                            #f 
                            "expected <field> or (<struct-type> <field>)"
                            field-spec)]))
                      (syntax->list #'(field-spec ...)))]
                [index+fieldspec
                 (map (lambda (field-spec)
                        (syntax-case field-spec ()
                          [(struct-type field)
                           (resolve-field static-info #'struct-type #'field)]))
                      field-specs)]
                [ordered-index+fieldspec+values
                 (mergesort
                  (map append 
                       index+fieldspec
                       (map list (syntax->list #'(value ...))))
                  (lambda (a b) (< (car a) (car b))))]
                [constructor (info:struct-constructor static-info)]
                )
           (unless total-field-count
             (raise-syntax-error #f "struct type has incomplete static information" #'type))
           (let loop ([info static-info])
             (cond [(boolean? info)
                    'okay]
                   [else
                    (let ([fields (map info:field-field (info:struct-field-infos info))])
                      (for-each 
                       (lambda (field)
                         (unless (ormap
                                  (lambda (fs)
                                    (and (module-identifier=? (info:struct-name info)
                                                              (caadr fs))
                                         (eq? field (syntax-e (cadadr fs)))))
                                  index+fieldspec)
                           (raise-syntax-error #f
                                               (format "field value not provided for ~s:~s"
                                                       (syntax-e (info:struct-name info))
                                                       field)
                                               stx)))
                       fields)
                      (loop (info:struct-super info)))]))
           (unless (= (length ordered-index+fieldspec+values) total-field-count)
             (raise-syntax-error #f "wrong number of constructor arguments" stx))
           (with-syntax ([((index fieldspec value) ...) ordered-index+fieldspec+values]
                         [constructor constructor])
             #'(make/h constructor (fieldspec value) ...))))]
      [else
       (raise-syntax-error #f "wtf?!" stx)]))
  
  (define-syntax make/h
    (syntax-rules ()
      [(_ constructor (fieldspec value) ...)
       (constructor value ...)]))
  
  (define-syntax (copy stx)
    (syntax-case stx ()
      [(_ type from-value (field-spec value) ...)
       #'(copy* type type from-value (field-spec value) ...)]))
  
  (define-syntax (copy* stx)
    (syntax-case stx ()
      [(_ type from-type from-expr (field-spec value) ...)
       #'#f]))
  
  )