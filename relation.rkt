#lang racket/base
(require (for-syntax racket/base
                     racket/set
                     syntax/parse
                     "private/relation-syntax.rkt"))
(provide define-relation
         relation-contains?
         relation?)

(define-for-syntax NOISY? #f)

(struct relation (fields ht updater))

(define none (gensym 'none))

(define-syntax (define-relation stx)
  (syntax-parse stx
    [(dr name0:id fl:field-list
         (~seq #:function (~var fn (fundep (syntax->list #'(fl.fld ...))))) ...
         (~var r (row (syntax->list #'(fl.fld ...)))) ...)
     (with-syntax ([(return-relation? relname ...)
                    (cond [(free-identifier=? #'name0 #'_)
                           #'(#f)]
                          [else
                           #'(#t name0)])])
       #'(define-values (relname ... fn.name ...)
           (relation* (fl.fld ...)
                      #f
                      [(fn.name (fn.arg ...) (fn.res ...)) ...]
                      [(r.e ...) ...]
                      return-relation?)))]))

#|
relation* performs the following optimizations:

- If a function has one argument and all values in the relation for
that field are eq-comparable constants, the function is implemented as
a case expression.

todo: update? and not return-relation? makes no sense
|#
(define-syntax (relation* stx)
  (syntax-case stx ()
    [(r* (fld ...)
         update?
         [(fn-name (fn-arg ...) (fn-rng ...)) ...]
         [r ...]
         return-relation?)
     (let* ([row-count (length (syntax->list #'(r ...)))]
            [flds (syntax->datum #'(fld ...))]              ;; (sym ...)
            [fn-names (syntax->list #'(fn-name ...))]       ;; (id ...)
            [fn-argss (syntax->datum #'((fn-arg ...) ...))] ;; ((sym ...) ...)
            [fn-rngss (syntax->datum #'((fn-rng ...) ...))] ;; ((sym ...) ...)
            [const-fld-table                    ;; sym => vector/#f
             (let ([t (make-hasheq)])
               (for ([fld (in-list flds)])
                 (hash-set! t fld (make-vector row-count)))
               t)]
            [const-fld-set-table                ;; sym => seteq/#f
             (let ([t (make-hasheq)])
               (for ([fld (in-list flds)])
                 (hash-set! t fld (seteq)))
               t)]
            [rows ;; ((oe ...) ...)
             (for/list ([r (in-list (syntax->list #'(r ...)))]
                        [ri (in-naturals)])
               (for/list ([e (in-list (syntax->list r))]
                          [fld (in-list flds)])
                 (let-values ([(ee oe) (syntax-local-expand-expression e)])
                   (let ([const? 
                          (syntax-case ee (quote)
                            [(quote d) (eq-able? (syntax->datum #'d)) #'d]
                            [_ #f])])
                     (cond [const?
                            (let ([v (hash-ref const-fld-table fld #f)]
                                  [s (hash-ref const-fld-set-table fld #f)])
                              (when v (vector-set! v ri (syntax->datum const?)))
                              (when s (hash-set! const-fld-set-table fld
                                                 (set-add s (syntax->datum const?)))))]
                           [else
                            (hash-set! const-fld-table fld #f)
                            (hash-set! const-fld-set-table fld #f)]))
                   oe)))]
            [opt-fun-table  ;; id => #t
             (let ([t (make-hasheq)])
               (for ([fn-name (in-list fn-names)]
                     [fn-args (in-list fn-argss)])
                 (when (and (= (length fn-args) 1)
                            (hash-ref const-fld-table (car fn-args) #f)
                            ;; no duplicates (ie no fun-dep violations)
                            (= row-count
                               (set-count (hash-ref const-fld-set-table (car fn-args)))))
                   (hash-set! t fn-name #t)))
               t)]

            [fld-vecs (generate-temporaries #'(fld ...))]
            [fld-indexes (build-list (length flds) values)]
            [fld-vec-table ;; sym => id
             (let ([t (make-hasheq)])
               (for ([fld (in-list flds)]
                     [fld-vec (in-list fld-vecs)])
                 (hash-set! t fld fld-vec))
               t)])

       (when NOISY?
         (for ([fn (in-list fn-names)])
           (if (hash-ref opt-fun-table fn #f)
               (eprintf "function ~a uses case expression\n" (syntax-e fn))
               (eprintf "function ~a uses hash\n" (syntax-e fn)))))

       (with-syntax ([((oe ...) ...) rows]
                     [row-count row-count]
                     [(fld-vec ...) fld-vecs]
                     [(fld-index ...) fld-indexes]
                     [((ht-fn-name (ht-fn-arg ...) (ht-fn-rng ...) ht-fn-ht) ...)
                      (for/list ([fn-name (in-list fn-names)]
                                 [fn-args (in-list (syntax->list #'((fn-arg ...) ...)))]
                                 [fn-rngs (in-list (syntax->list #'((fn-rng ...) ...)))]
                                 #:when (not (hash-ref opt-fun-table fn-name #f)))
                        (list fn-name fn-args fn-rngs (generate-temporary)))]
                     [((opt-fn-name (opt-fn-arg) (opt-fn-rng ...) opt-fn-ht
                                    (opt-fn-rng-vec ...)
                                    ((opt-fn-datum row-index) ...)) ...)
                      (for/list ([fn-name (in-list fn-names)]
                                 [fn-args (in-list (syntax->list #'((fn-arg ...) ...)))]
                                 [fn-rngs (in-list (syntax->list #'((fn-rng ...) ...)))]
                                 #:when (hash-ref opt-fun-table fn-name #f))
                        (list fn-name fn-args fn-rngs (generate-temporary)
                              (for/list ([rng (in-list (syntax->datum fn-rngs))])
                                (hash-ref fld-vec-table rng))
                              (for/list ([datum
                                          (in-vector
                                           (hash-ref const-fld-table
                                                     (car (syntax->datum fn-args))))]
                                         [i (in-naturals)])
                                (list datum i))))])

         #'(let ([rel? return-relation?]
                 [add? update?]
                 [rows (vector (vector oe ...) ...)]
                 [ht-fn-ht (make-hash)] ...)
             (let* ([main-ht
                     ;; Only need relation table if building relation obj
                     ;; *or* if no (stricter) fun-deps to check for duplicates.
                     (and (or rel? (null? '(ht-fn-name ... opt-fn-name ...)))
                          (make-hash))]
                    [opt-fn-ht
                     ;; opt-funs need table only to check fun-dep violations,
                     ;; and new violations are only possible if add? = #t
                     (and add? (make-hash))]
                    ...
                    [fld-vec (make-vector row-count)] ...)

               (define (update! row)
                 (let-values ([(fld ...) (vector->values row)])
                   (add/check-fun-dep
                    ht-fn-ht (vector ht-fn-arg ...) (vector ht-fn-rng ...)
                    'ht-fn-name '(ht-fn-arg ...) '(ht-fn-rng ...))
                   ...
                   (when add?
                     (add/check-fun-dep
                      opt-fn-ht (vector opt-fn-arg) (vector opt-fn-rng ...)
                      'opt-fn-name '(opt-fn-arg) '(opt-fn-rng ...))
                     ...
                     (void))
                   (when main-ht
                     (add/check-fun-dep
                      main-ht (vector fld ...) #t #f #f #f))))
               (define (ht-fn-name ht-fn-arg ... [default none])
                 (do-lookup 'ht-fn-name ht-fn-ht (vector ht-fn-arg ...) default))
               ...

               (define (opt-fn-name opt-fn-arg [default none])
                 (let ([index
                        (case opt-fn-arg
                          ((opt-fn-datum) 'row-index)
                          ...
                          (else #f))])
                   (cond [index
                          (if (null? '(opt-fn-rng-vec ...))
                              #t
                              (values (vector-ref opt-fn-rng-vec index) ...))]
                         [add?
                          (do-lookup 'opt-fn-name opt-fn-ht (vector opt-fn-arg) default)]
                         [else
                          (do-default 'opt-fn-name default (vector opt-fn-arg))])))
               ...

               (for ([row (in-vector rows)]
                     [i (in-naturals)])
                 (update! row)
                 (vector-set! fld-vec i (vector-ref row fld-index)) ...)
               (if rel?
                   (values (relation '(fld ...)  main-ht (and add? update!))
                           fn-name ...)
                   (values fn-name ...))))))]))

(define (add/check-fun-dep ht key value name arg-fields rng-fields)
  (unless (eq? none (hash-ref ht key none))
    (if name
        (error 'define-relation
               "functional dependency ~a violated: duplicate for key ~.s"
               (if (null? rng-fields)
                   `(,name : ,@arg-fields -> #t)
                   `(,name : ,@arg-fields -> ,@rng-fields))
               (vector->list key))
        (error 'define-relation
               "duplicate entry in relation: ~.s" (vector->list key))))
  (hash-set! ht key value))

(define (do-lookup fsym ht key default)
  (let ([result (hash-ref ht key none)])
    (if (eq? result none)
        (do-default fsym default key)
        (if (zero? (vector-length result))
            #t
            (vector->values result)))))

(define (do-default fsym default key)
  (cond [(eq? default none)
         (error fsym "no entry for key: ~.s" (vector->list key))]
        [(procedure? default) (default)]
        [else default]))

;; ----

#|
(define (relation-add! rel row)
  (let ([update! (relation-updater rel)])
    (unless update!
      (error 'relation-add! "relation does not allow additions: ~e" rel))
    (update! (list->vector row))))
|#

(define (relation-contains? rel row)
  (and (hash-ref (relation-ht rel) (list->vector row) #f) #t))

;; ----

#|
(define-relation _ (n d)
  #:function (get-d : n -> d)
  #:function (get-n : d -> n)
  (0 "zero")
  (1 "one")
  (2 "two"))
|#
