
(module struct mzscheme
  (require-for-syntax "stx.ss"
                      "private/struct-helper.ss"
                      "private/compat-for-399.ss"
                      (lib "etc.ss")
		      (lib "list.ss"))
  (require "private/struct-props.ss"
           "private/structlike.ss")
  (provide define-struct*
           define-struct-property
           define-struct-like)

  ;; define-struct* SYNTAX
  (define-syntax (define-struct* stx)
    (syntax-case stx ()
      [(_ (type super-type) [field-decl ...] decl ...)
       (and (identifier? #'type) (identifier? #'super-type))
       #'(define-struct* type [field-decl ...] (super super-type) decl ...)]
      [(_ type [field-decl ...] decl ...)
       (identifier? #'type)
       (let* ([field-decls 
               (map (mk-parse-field-decl #'type) 
                    (syntax->list #'(field-decl ...)))]
              [decls (syntax->list #'(decl ...))]
              [info (create-info #'type decls field-decls)])
         (let ([init-field-k (length (info-init-fields info))]
               [auto-field-k (length (info-auto-fields info))]
               [props-kv (info-props info)]
               [fdecls (info2-fdecls info)])
           #`(begin
               (define-values #,(info-core-names info)
                 (let-values 
                  ([(struct:x make-x x? x-ref x-set!)
                    (make-struct-type 
                     'type
                     #,(info-super-struct info)
                     #,init-field-k
                     #,auto-field-k
                     #,(info-auto-v info)
                     #,(with-syntax ([(prop-key ...)
                                      (map car props-kv)]
                                     [(prop-val ...)
                                      (map cdr props-kv)])
                                    #'(list (cons prop-key prop-val) ...))
                     #,(cond [(info-insp info)
                              => values]
                             [(info-lookup info 'transparent)
                              #'#f]
                             [else #'(current-inspector)])
                     #,(info-proc-spec info)
                     '#,(info-imm-k-list info)
                     #,(info-guard info))])
                  (values struct:x
                          make-x
                          x?
                          x-ref
                          x-set!)))
               (define-values #,(info-ref-names info)
                 (values #,@(map (lambda (ref-field ref-posn)
                                   #`(make-struct-field-accessor 
                                      #,(info-name:gen-accessor info)
                                      #,ref-posn
                                      '#,ref-field))
                                 (info-ref-fields info)
                                 (info-ref-posns info))))
               (define-values #,(info-mut-names info)
                 (values #,@(map (lambda (mut-field mut-posn)
                                   #`(make-struct-field-mutator
                                      #,(info-name:gen-mutator info)
                                      #,mut-posn
                                      '#,mut-field))
                                 (info-mut-fields info)
                                 (info-mut-posns info))))
               (define-syntax type
                 (let ([c (syntax-local-certifier)])
                   (list-immutable
                    #'#,(info-name:struct-record info)
                    (c #'#,(info-name:constructor info))
                    (c #'#,(info-name:predicate info))
                    #,(with-syntax ([(accessor ...)
                                     (reverse (map field-decl-ref fdecls))])
                        #'(list-immutable (c #'accessor) ...))
                    #,(with-syntax ([(mutator ...)
                                     (reverse (map field-decl-mut fdecls))])
                        #'(list-immutable (c #'mutator) ...))
                    #,(let ((super (info-super info))
                            (super-struct (info-super-struct info)))
                        (cond [super
                               #`(c #'#,super)]
                              [super-struct
                               #'#f]
                              [else
                               #'#t])))))
               )))]))
  )
