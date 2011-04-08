
(module class-ct mzscheme
  (require "../struct.ss")
  (require-for-template mzscheme
                        (lib "class.ss"))
  (provide (struct static-interface (dynamic members))
           (struct checked-binding (dynamic iface)))
  
  (define-struct* static-interface [dynamic members]
    [#:procedure
     (lambda (self stx)
       (syntax-case stx ()
         [(ifname . args)
          #'((begin ifname) . args)]
         [ifname
          (identifier? #'ifname)
          (static-interface-dynamic self)]))])
  
  (define-struct-like checked-binding [dynamic iface]
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! var expr)
          #`(let ([newval expr])
              (unless (is-a? newval #,(static-interface-dynamic iface))
                (error 'check "interface check failed on: ~e" newval))
              (set! #,dynamic newval))]
         [(var . args)
          (datum->syntax-object stx (cons #'(begin var) #'args) stx stx)]
         [var
          (identifier? #'var)
          dynamic]
         [else
          (raise-syntax-error #f "oops" stx)]))))
  )
