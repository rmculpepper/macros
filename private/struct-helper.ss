(module struct-helper mzscheme
  (require (lib "list.ss")
           "../stx.ss"
           "compat-for-399.ss")
  (require-for-template mzscheme)
  (provide (all-defined))

  ;; A field-decl is (make-field-decl id id id num boolean boolean)
  (define-struct field-decl (field ref mut posn immutable? auto?) (make-inspector))

  (define-struct info2 (type fdecls ht))
  ;; type : identifier
  ;; fdecls : (list-of field-decl)
  ;; ht : hashtable[symbol => value]
  ;;  'super => identifier/tf
  ;;  'super-struct => identifier/tf
  ;;  'constructor => identifier
  ;;  'predicate => identifier
  ;;  '...

  (define (fetch-super-struct type)
    (define (err)
      (raise-syntax-error 'define-struct*
                          "not defined as a struct"
                          type))
    (let ([struct-info (syntax-local-value type err)])
      (car (get-struct-info-as-list struct-info))))

  (define (info-lookup info key)
    (hash-table-get (info2-ht info) key (lambda () #f)))

  (define (info-lookup-list info key)
    (hash-table-get (info2-ht info) key (lambda () null)))

  (define info-put-fresh!
    (case-lambda 
      [(info key value fail)
       (let/ec k
         (hash-table-get (info2-ht info)
                         key
                         (lambda () 
                           (k (hash-table-put! (info2-ht info) key value))))
         (fail))]
      [(info key value)
       (info-put-fresh! 
        info
        key 
        value
        (lambda ()
          (error 'info-put-fresh! "option ~s specified twice" key)))]))
  
  (define (info-add! info key value)
    (hash-table-put! (info2-ht info) 
                     key 
                     (cons value 
                           (info-lookup-list info key))))
  
  ;; An identifier/tf is one of
  ;;   - identifier
  ;;   - #t
  ;;   - #f
  
  ;; identifier/tf? : any -> boolean
  (define (identifier/tf? stx)
    (or (identifier? stx) (stx-boolean? stx)))

  ;; id/tf : identifier/tf identifier/tf -> identifier/tf
  (define (id/tf stx stx2)
    (cond [(identifier? stx)
           stx]
          [(eq? (syntax-e stx) #t)
           stx2]
          [else #f]))
  
  (define (memq/f item items)
    (and items (memq item items)))
  
  ;; mk-parse-field-decl : identifier -> syntax -> field-decl
  (define (mk-parse-field-decl name-id)
    (define (parse-field-decl stx)
      (syntax-case stx ()
        [(field (flag ...) ref mut)
         (begin
           (unless (identifier? #'field)
             (raise-syntax-error 'define-struct*
                                 "field name must be identifier"
                                 #'field))
           (unless (identifier/tf? #'ref)
             (raise-syntax-error 'define-struct*
                                 "accessor name must be identifier, #t, or #f"
                                 #'ref))
           (unless (identifier/tf? #'mut)
             (raise-syntax-error 'define-struct*
                                 "mutator name must be identifier, #t, or #f"
                                 #'ref))
           (let ((flags (syntax-object->datum #'(flag ...))))
             (make-field-decl
              #'field
              (id/tf #'ref (name:ref name-id #'field))
              (id/tf #'mut (name:mut name-id #'field))
              #f
              (memq '#:immutable flags)
              (memq '#:auto flags))))]
        [(field (flag ...) ref)
         (parse-field-decl #'(field (flag ...) ref #t))]
        [(field (flag ...))
         (parse-field-decl
          #`(field 
             (flag ...)
             #t
             #t))]
        [field
         (identifier? #'field)
         (parse-field-decl
          #`(field () #t #t))]
        [_
         (raise-syntax-error 'define-struct* "bad field declaration" stx)]))
    parse-field-decl)
  
  (define (mk-parse-decl info)
    (define (check!/1 info blamestx sym)
      (when (info-lookup info sym)
        (raise-syntax-error 'define-struct*
                            (format "option repetition or conflict with earlier '~s" sym)
                            blamestx)))
    (define (check! info blamestx . syms)
      (for-each (lambda (s) check!/1 info blamestx s) syms))
    (define (parse-decl stx)
      (syntax-case stx ()
        [(#:super type)
         (identifier? #'type)
         (begin (check! info stx 'super 'super-struct)
                (info-put-fresh! info 'super #'type)
                (info-put-fresh! info 'super-struct (fetch-super-struct #'type)))]
        [(#:super-struct value)
         (begin (check! info stx 'super-struct)
                (info-put-fresh! info 'super-struct #'value))]
        [(#:auto-value value)
         (begin (check! info stx 'auto-value)
                (info-put-fresh! info 'auto-value #'value))]
        [(#:property key value)
         (info-add! info 'properties (cons #'key #'value))]
        [(#:inspector value)
         (begin (check! info stx 'transparent 'inspector)
                (info-put-fresh! info 'inspector #'value))]
        [#:transparent
         (begin (check! info stx 'transparent 'inspector)
                (info-put-fresh! info 'transparent #t))]
        [(#:procedure proc)
         (begin (check! info stx 'procedure-field 'procedure)
                (info-put-fresh! info 'procedure #'proc)
                (info-put-fresh! info 'procedure-spec #'proc))]
        [(#:procedure-field field)
         (identifier? #'field)
         (begin (check! info stx 'procedure-field 'procedure)
                (info-put-fresh!
                 info
                 'procedure-spec
                 (let loop ((fdecls (info2-fdecls info)))
                   (cond [(null? fdecls)
                          (raise-syntax-error 'define-struct*
                                              "procedure-field not in field set"
                                              stx)]
                         [(module-identifier=? #'field (field-decl-field (car fdecls)))
                          (field-decl-posn (car fdecls))]
                         [else (loop (cdr fdecls))])))
                (info-put-fresh! info 'procedure-field #'field))]
        [(#:guard proc)
         (begin (check! info stx 'guard)
                (info-put-fresh! info 'guard #'proc))]
        [#:omit-define-values
         (info-add! info 'options 'omit-define-values)]
        [#:omit-define-syntaxes
         (info-add! info 'options 'omit-define-syntaxes)]
        [_
         (raise-syntax-error 'define-struct* "unknown option" stx)]))
    parse-decl)

  ;; add-positions-to-field-decls! : (list-of field-decl) -> void
  (define (add-positions-to-field-decls! fdecls)
    (let loop ((fdecls fdecls) (posn 0) (first-auto #f))
      (when (pair? fdecls)
        (let ((fdecl (car fdecls)))
          (set-field-decl-posn! fdecl posn)
          (when (and first-auto (not (field-decl-auto? fdecl)))
            (raise-syntax-error 'define-struct*
                                "non-auto field after auto field"
                                (field-decl-field fdecl)))
          (loop (cdr fdecls)
                (add1 posn)
                (or first-auto (if (field-decl-auto? fdecl) posn #f)))))))

  (define (new-info type fdecls)
    (make-info2 type fdecls (make-hash-table)))

  (define (create-info type decls fdecls)
    (let ((info (new-info type fdecls)))
      (add-positions-to-field-decls! fdecls)
      (for-each (mk-parse-decl info) decls)
      (when (and (info-include-subst? info) (pair? (info-auto-fields info)))
        (error 'define-struct* "cannot define substitutions with auto-fields"))
      info))

  ;; info accessors
  (define (info-super info)
    (info-lookup info 'super))

  (define (info-super-struct info)
    (info-lookup info 'super-struct))

  (define (info-auto-k info)
    (length (filter field-decl-auto? (info2-fdecls info))))

  (define (info-auto-v info)
    (info-lookup info 'auto-value))

  (define (info-props info)
    (info-lookup-list info 'properties))

  (define (info-insp info)
    (info-lookup info 'inspector))

  (define (info-proc-spec info)
    (info-lookup info 'procedure-spec))

  (define (info-imm-k-list info)
    (map field-decl-posn 
         (filter field-decl-immutable? 
                 (info2-fdecls info))))

  (define (info-guard info)
    (info-lookup info 'guard))

  (define (info-ref-fields info)
    (map field-decl-field (filter field-decl-ref (info2-fdecls info))))

  (define (info-ref-posns info)
    (map field-decl-posn (filter field-decl-ref (info2-fdecls info))))

  (define (info-ref-names info)
    (map field-decl-ref (filter field-decl-ref (info2-fdecls info))))

  (define (info-mut-fields info)
    (map field-decl-field (filter field-decl-mut (info2-fdecls info))))

  (define (info-mut-posns info)
    (map field-decl-posn (filter field-decl-mut (info2-fdecls info))))
  
  (define (info-mut-names info)
    (map field-decl-mut (filter field-decl-mut (info2-fdecls info))))

  (define (info-options info)
    (info-lookup info 'options))

  (define (info-init-fields info)
    (filter (lambda (fdecl) (not (field-decl-auto? fdecl)))
            (info2-fdecls info)))

  (define (info-auto-fields info)
    (filter (lambda (fdecl) (field-decl-auto? fdecl))
            (info2-fdecls info)))

  (define (info-include-define-values? info)
    (not (memq/f 'omit-define-values (info-options info))))

  (define (info-include-static-info? info)
    (not (memq/f 'omit-define-syntaxes (info-options info))))

  (define (info-include-subst? info)
    (memq/f 'include-subst (info-options info)))

  (define (info-include-clone? info)
    (memq/f 'include-clone (info-options info)))

  (define (info-name:struct-record info)
    (let ((type (info2-type info)))
      (name:struct-record type)))

  (define (info-name:constructor info)
    (let ((type (info2-type info)))
      (name:make type)))

  (define (info-name:predicate info)
    (let ((type (info2-type info)))
      (name:pred type)))

  (define (info-name:gen-accessor info)
    (let ((type (info2-type info)))
      (name:gen-accessor type)))

  (define (info-name:gen-mutator info)
    (let ((type (info2-type info)))
      (name:gen-mutator type)))

  (define (info-core-names info)
    (let ((type (info2-type info)))
      (list (info-name:struct-record info)
            (info-name:constructor info)
            (info-name:predicate info)
            (info-name:gen-accessor info)
            (info-name:gen-mutator info))))

  (define (name:make type)
    (datum->syntax-object type (symbol-append 'make- type)))
  (define (name:pred type)
    (datum->syntax-object type (symbol-append type '?)))
  (define (name:struct-record type)
    (datum->syntax-object type (symbol-append 'struct: type)))
  (define (name:gen-accessor type)
    (datum->syntax-object type (symbol-append type '-ref)))
  (define (name:gen-mutator type)
    (datum->syntax-object type (symbol-append type 'set!)))
  (define (name:ref type field)
    (datum->syntax-object type (symbol-append type '- field)))
  (define (name:mut type field)
    (datum->syntax-object type (symbol-append 'set- type '- field '!)))
  
  )
