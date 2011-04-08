(module struct-subst mzscheme
  (require "struct-props.ss")
  (require-for-syntax "../stx.ss"
                      "struct-ct.ss"
                      (lib "list.ss"))
  (provide define-struct-substs)
  
  (define-struct subst-info (maker init-k ref set super))
  
  (define-struct-property struct:subst
    (lambda (propval info-list)
      (let ([no-super? (car propval)]
            [constructor (cdr propval)]
            [init-k (list-ref info-list 1)]
            [auto-k (list-ref info-list 2)]
            [ref (list-ref info-list 3)]
            [set (list-ref info-list 4)]
            [super-struct-type (list-ref info-list 6)]
            [skipped? (list-ref info-list 7)])
	(printf "subst:struct got: ~s, ~s~n"
		propval info-list)
        (unless (or no-super? 
                    (and (struct:subst? super-struct-type) (not skipped?)))
          (error 'subst "super struct type does not support substitution"))
        (unless (zero? auto-k)
          (error 'subst "substitution and auto fields not compatible"))
        (make-subst-info constructor init-k ref set 
                         (and super-struct-type 
                              (struct:subst-value super-struct-type))))))

  (define-syntax (define-struct-substs stx)
    (syntax-case stx ()
      [(_ type struct-type (field ...))
       (with-syntax ([(subst ...)
                      (map (lambda (f) (implicit-id #'type (sym+ 'subst- #'type '- f)))
                           (syntax->list #'(field ...)))]
                     [(f-index ...)
                      (let loop ([i 0] [fs (syntax->list #'(field ...))])
                        (if (null? fs)
                            null
                            (cons i (loop (add1 i) (cdr fs)))))])
         #'(begin (define (subst obj newval)
                    (substitute 'subst struct-type obj f-index newval))
                  ...))]))

  (define (substitute/super-chain si)
    (if si
        (cons si (substitute/super-chain (subst-info-super si)))
        null))
  
  (define (extract-fields/subst obj si repli replv)
    (let ([ref (subst-info-ref si)]
          [k (subst-info-init-k si)])
      (let loop ([i 0])
        (cond [(= i k) null]
              [(and repli (= repli i))
               (cons replv (loop (add1 i)))]
              [else (cons (ref obj i) (loop (add1 i)))]))))
  
  (define (extract-fields obj si)
    (let ([ref (subst-info-ref si)]
          [k (subst-info-init-k si)])
      (let loop ([i 0])
        (cond [(= i k) null]
              [else (cons (ref obj i) (loop (add1 i)))]))))
  
  (define (substitute called-as struct-type obj f-index newval)
    (when (or (struct-type? (not (struct:subst? obj))))
      (raise-type-error called-as "substitutable struct" obj))
    (let ([subst-info (struct:subst-value obj)]
          [repl-subst-info (struct:subst-value struct-type)])
      (unless (subst-info? subst-info)
        (error 'substitute "internal error: ~s" subst-info))
      (let* ([constructor (subst-info-maker subst-info)]
             [chain (substitute/super-chain subst-info)]
             [fieldss (map (lambda (si)
                             (if (eq? si repl-subst-info)
                                 (extract-fields/subst obj si f-index newval)
                                 (extract-fields obj si)))
                           chain)])
        (apply constructor (apply append fieldss)))))
  
  (define-syntax (define-extending-constructors stx)
    (syntax-case stx ()
      [(_ name)
       (let ((si (static-struct-info #'name)))
	 (define constructor (info:struct-constructor si))
	 (define names (map-static-struct-frames info:struct-name si))
	 (define gen-accessors 
	   (map-static-struct-frames info:struct-gen-accessor si))
	 (define initfss
	   (map-static-struct-frames
	    (lambda (info)
	      (map info:field-field
		   (filter info:field-init? 
			   (info:struct-field-infos info))))
	    si))
	 (define (constructor/given pname)
	   (implicit-id #'name
			(sym+ 'make- #'name '/ pname)))
	 (define (def make/pname grefs initfsf args)
	   (with-syntax ([formals (generate-temporaries (car initfss))]
			 [(init-k ...) (map length (cdr initfss))]
			 [(gref ...) (cdr grefs)])
             #`(define (#,make/pname basis . formals)
		 (apply #,constructor
			(apply append
			       (reverse
				(list . formals)
				(map/n (lambda (n) (gref basis n))
				       init-k)
				...))))))
	 (define stxs
	   (let loop ((pnames names)
		      (grefs gen-accessors)
		      (initfss initfss)
		      (args 0))
	     (if (null? pnames)
		 null
		 (cons (def (constructor/given (car pnames))
			    grefs
			    initfss
			    args)
		       (loop (cdr pnames)
			     (cdr grefs)
			     (cdr initfss)
			     (+ args (length (car initfss))))))))
	 #`(begin #,@stxs))]))
  
  )  