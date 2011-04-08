
(module struct-ct mzscheme
  (require-for-syntax "../stx.ss")
  (provide register-struct-info
           static-struct-info
	   map-static-struct-frames
	   (struct info:field (field accessor mutator init?))
	   (struct base-info:struct
		   (descriptor constructor predicate field-infos
                    super complete?))
	   (struct info:struct
		   (name descriptor super constructor predicate
		    gen-accessor gen-mutator field-infos props total-field-count)))
  
  ;; static-info-table : weak-hash-table[? => info:struct]
  ;; Shared compile-time table
  (define static-info-table (make-hash-table 'weak))
  
  ;; type info:struct
  ;; Complete static description of a struct type
  ;;   super : identifier | #t | #f
  ;;   {name descriptor constructor predicate gen-accessor gen-mutator} : identifier
  ;;   field-infos : (list-of info:field)
  ;;   props : ???
  (define-struct info:struct 
    (name descriptor super constructor predicate 
     gen-accessor gen-mutator field-infos props total-field-count))

  ;; type base-info:struct
  ;; Basic info (present in define-struct by default)
  (define-struct base-info:struct 
    (descriptor constructor predicate field-infos super complete?))

  ;; type info:field
  (define-struct info:field (field accessor mutator init?))
  
  ;; syntax register-struct-info
  ;; Registers the struct info in the static table
  ;; Example
  ;;   (register-struct-info A struct:A #f make-A A? A-ref A-set!
  ;;     [(a1 A-a1 set-A-a1! #t) (a2 A-a2 set-A-a2! #t)])
  (define-syntax (register-struct-info stx)
    (syntax-case stx ()
      [(_ name descriptor super constructor predicate gen-accessor gen-mutator
          [(field accessor mutator init?) ...])
       (with-syntax ([std-info-expr
                      #`(let ((c (syntax-local-certifier)))
                          (list-immutable (c #'descriptor)
                                          (c #'constructor)
                                          (c #'predicate)
                                          #,(with-syntax ([(accessor ...)
                                                           (reverse 
                                                            (syntax->list
                                                             #'(accessor ...)))])
                                              #'(list-immutable (c #'accessor) ...))
                                          #,(with-syntax ([(mutator ...)
                                                           (reverse
                                                            (syntax->list
                                                             #'(mutator ...)))])
                                              #'(list-immutable (c #'mutator) ...))
                                          (c #'super)))]
                     [super-info-expr
                      (cond [(identifier? #'super)
                             #'(static-struct-info #'super)]
                            [(stx-false? #'super)
                             #'#f]
                            [(stx-true? #'super)
                             #'#t])])
         #`(let ([std-info std-info-expr]
                 [super-info super-info-expr])
             (hash-table-put!
              static-info-table std-info
              (let ((c (syntax-local-certifier)))
                (make-info:struct
                 (c #'name)
                 (c #'descriptor)
                 super-info
                 (c #'constructor)
                 (c #'predicate)
                 (c #'gen-accessor)
                 (c #'gen-mutator)
                 (list-immutable
                  (make-info:field 'field 
                                   (c #'accessor)
                                   (c #'mutator) init?)
                  ...)
                 null
                 #,(cond [(identifier? #'super)
                          #`(if (info:struct-total-field-count super-info)
                                (+ (info:struct-total-field-count super-info)
                                   #,(length (syntax->list #'(field ...))))
                                #'#f)]
                         [(stx-true? #'super)
                          (length (syntax->list #'(field ...)))]
                         [(stx-false? #'super)
                          #'#f]))))
             std-info))]))

  ;; static-struct-info : identifier -> info:struct
  (define static-struct-info
    (case-lambda
      [(name thunk1 thunk2)
       (let ([std-info (syntax-local-value name thunk1)])
         (hash-table-get static-info-table std-info thunk2))]
      [(name thunk)
       (static-struct-info name thunk 
                           (lambda () 
                             (raise-syntax-error 
                              'define-struct*
                              "super struct type not defined via define-struct*"
                              name)))]
      [(name)
       (static-struct-info name 
                           (lambda () (raise-syntax-error 'define-struct*
                                                          "super struct type not defined" name)))]))
  
  ;; map-static-struct-frames : (info:struct -> 'a) info:struct -> (list-of 'a)
  (define (map-static-struct-frames f info)
    (let loop ((info info))
      (cond [(eq? info #t)
	     ;; Top of ancestor tree
	     null]
	    [(info:struct? info)
	     (cons (f info) (loop (info:struct-super info)))]
	    [else
	     ;;Unknown
	     (error 'map-static-struct-frames
		    "struct has unknown ancestor: ~s" info)])))
  
  )
