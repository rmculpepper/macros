
(module class-iop mzscheme
  (require (lib "class.ss"))
  (require-for-syntax "private/class-ct.ss"
                      "stx.ss")
  (provide define-interface
           define-static-interface
           
           send:
           send*:
           send/apply:
           
           define:
           lambda:
           init:
           init-private:)
  
  ;; Configuration
  (define-for-syntax warn-on-dynamic-interfaces? #f)
  (define-for-syntax warn-on-dynamic-object-check-generation? #f)
  (define-for-syntax warn-on-dynamic-object-check? #f)
  (define-for-syntax define-dotted-names #f)
  
  ;; define-interface SYNTAX
  ;; (define-interface NAME (IDENTIFIER ...))
  ;; Defines NAME as an interface.
  (define-syntax (define-interface stx)
    (syntax-case stx ()
      [(di name (mname ...))
       #'(define-static-interface name
           (let ([name (interface () mname ...)]) name)
           (mname ...))]))

  ;; define-static-interface SYNTAX
  ;; (define-static-interface NAME EXPR (IDENTIFIER ...))
  ;; Defines NAME as a static interface containing the names listed.
  ;; The EXPR is used as the dynamic componenent of the interface, and 
  ;; it should contain a superset of the names listed.
  (define-syntax (define-static-interface stx)
    (syntax-case stx ()
      [(dsi name dynamic-interface (mname ...))
       (and (identifier? #'name)
            (andmap identifier? (syntax->list #'(mname ...))))
       (with-syntax ([(dynamic-name) (generate-temporaries #'(name))])
         #'(begin (define dynamic-name
                    (let ([dynamic-name dynamic-interface])
                      (for-each 
                       (lambda (m)
                         (unless (method-in-interface? m dynamic-name)
                           (error 'name "dynamic interface missing method '~s'" m)))
                       '(mname ...))
                      dynamic-name))
                  (define-syntax name
                    (make-static-interface #'dynamic-name '(mname ...)))))]))
  
  ;; Checked send
  
  (define-syntax (send: stx)
    (syntax-case stx ()
      [(send: obj iface method . args)
       (and (identifier? #'iface) (identifier? #'method))
       #`(begin (check-method<-interface method iface)
                #,(syntax/loc stx 
                    (send (check-object<:interface send: obj iface) 
                          method . args)))]))
  
  (define-syntax (send*: stx)
    (syntax-case stx ()
      [(send*: obj iface (method . args) ...)
       (and (identifier? #'iface) (andmap identifier? (syntax->list #'(method ...))))
       #`(begin (check-method<-interface method iface) ...
                #,(syntax/loc stx 
                    (send* (check-object<:interface send*: obj iface)
                      (method . args) ...)))]))
  
  (define-syntax (send/apply: stx)
    (syntax-case stx ()
      [(send/apply: obj iface method . args)
       (and (identifier? #'iface) (identifier? #'method))
       #`(begin (check-method<-interface method iface)
                #,(syntax/loc stx 
                    (send/apply (check-object<:interface send/apply obj iface)
                                method . args)))]))
  
  ;;
  
  ;; check-method<-interface SYNTAX
  (define-syntax (check-method<-interface stx)
    (syntax-case stx ()
      [(sci method iface-expr)
       (let ([si (syntax-local-value #'iface (lambda () #f))])
         (if (and si (static-interface? si))
             (begin
               (unless (member (syntax-e #'method) (static-interface-members si))
                 (raise-syntax-error 'checked-send
                                     "method not in static interface"
                                     #'method))
               #''okay)
             (begin (when warn-on-dynamic-interfaces?
                      (printf "dynamic interface check: ~s,~s: method: ~a~n"
                              (syntax-source #'method)
                              (syntax-line #'method)
                              (syntax-e #'method)))
                    #`(let ([iface iface-expr])
                        (unless (method-in-interface? 'method iface)
                          (error
                           'checked-send
                           "interface does not contain method '~a': ~e"
                           'method
                           iface))))))]))
  
  ;; check-object<:interface SYNTAX
  (define-syntax (check-object<:interface stx)
    (syntax-case stx ()
      [(coi for-whom obj iface)
       (and (identifier? #'obj))
       (let ([obj-ref (syntax-local-value #'obj (lambda () #f))]
             [si (syntax-local-value #'iface (lambda () #f))])
         (if (and (checked-binding? obj-ref)
                  (static-interface? si)
                  (eq? (checked-binding-iface obj-ref) si))
             #'obj
             (begin
               (when warn-on-dynamic-object-check?
                 (printf "dynamic object check: ~s,~s~n"
                         (syntax-source #'obj)
                         (syntax-line #'obj)))
               #'(dynamic:check-object<:interface 'for-whom obj iface))))]
      [(coi for-whom obj iface)
       (begin
         (when warn-on-dynamic-object-check-generation?
           (printf "dynamic object check: ~s,~s~n"
                   (syntax-source #'obj)
                   (syntax-line #'obj)))
         #'(dynamic:check-object<:interface 'for-whom obj iface))]))
  
  (define (dynamic:check-object<:interface for-whom obj iface)
    (unless (is-a? obj iface)
      (error for-whom "interface check failed on: ~e" obj))
    (let-syntax ([x (lambda (stx)
                      (if warn-on-dynamic-object-check?
                          #'(printf "dynamic: object check passed~n")
                          #'(void)))])
      x)
    obj)
  
  ;;
  
  (define-syntax (define: stx)
    (syntax-case stx ()
      [(define: name iface expr)
       (and (identifier? #'name) (identifier? #'iface))
       (let ([si (syntax-local-value #'iface (lambda () #f))])
         (unless (static-interface? si)
           (raise-syntax-error #f "not a static interface" #'iface))
         (with-syntax ([(name-internal) (generate-temporaries #'(name))]
                       [(method ...) (static-interface-members si)]
                       [(name.method ...)
                        (map (lambda (m)
                               (datum->syntax-object #'name (symbol-append #'name '|.| m)))
                             (static-interface-members si))])
           #`(begin (define name-internal
                      (check-object<:interface define: expr iface))
                    (define-syntax name
                      (make-checked-binding
                       #'name-internal
                       (syntax-local-value #'iface)))
                    #,(if define-dotted-names
                          #'(begin
                              (define-syntax name.method
                                (syntax-rules ()
                                  [(name.method . args)
                                   (send: name iface method . args)]))
                              ...)
                          #'(begin)))))]
      [(define: (f . args) . body)
       (and (identifier? #'f))
       #'(define f (lambda: args . body))]))

  (define-syntax (lambda: stx)
    (define (arg->define stx temp)
      (syntax-case stx ()
        [(arg : iface) 
         (and (identifier? #'arg)
              (eq? ': (syntax-e #':)))
         #`(define: arg iface #,temp)]
        [arg
         (identifier? #'arg)
         #`(define-syntax arg (make-rename-transformer #'#,temp))]))
    (syntax-case stx ()
      [(lambda: (arg ...) . body)
       (let ([temporaries (generate-temporaries #'(arg ...))])
         (with-syntax ([(temp ...) temporaries]
                       [(checked-definition ...)
                        (map arg->define
                             (syntax->list #'(arg ...))
                             temporaries)])
           #'(lambda (temp ...)
               (let ()
                 checked-definition ...
                 (let () . body)))))]))
  
  (define-syntax (init: stx)
    (syntax-case stx ()
      [(init: (name iface) ...)
       #'(begin (init1: name iface) ...)]))
  
  (define-syntax (init1: stx)
    (syntax-case stx ()
      [(init1: name iface)
       (with-syntax ([(name-internal) (generate-temporaries #'(name))])
         #'(begin (init (name name-internal))
                  (void (check-object<:interface init: name-internal iface))
                  (define-syntax name 
                    (make-checked-binding
                     #'name-internal
                     (syntax-local-value #'iface)))))]))
  
  (define-syntax (init-private stx)
    (syntax-case stx ()
      [(init-private form ...)
       #'(begin (init-private1 form) ...)]))
  
  (define-syntax (init-private1 stx)
    (syntax-case stx ()
      [(init-private1 id)
       (identifier? #'id)
       (with-syntax ([(id-internal) (generate-temporaries #'(id))])
         #'(begin (init (id-internal id))
                  (define id id-internal)))]))
  
  (define-syntax (init-private: stx)
    (syntax-case stx ()
      [(init-private: (name iface) ...)
       #'(begin (init-private1: name iface) ...)]))
  
  (define-syntax (init-private1: stx)
    (syntax-case stx ()
      [(init-private1: name iface)
       (identifier? #'id)
       (with-syntax ([(id-internal) (generate-temporaries #'(id))])
         #'(begin (init (id-internal name))
                  (define: name iface id-internal)))]))
  
  )
