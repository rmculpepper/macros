
(module schemepp mzscheme
  
  (require (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax")
           (lib "moddep.ss" "syntax")
           (lib "toplevel.ss" "syntax")
           (lib "pretty.ss"))
  (provide schemepp
           schemepp/file)

  ;; schemepp : -> void
  (define (schemepp)
    (for-each pretty-print (schemepp/stxs (read-all-syntaxes))))
  
  ;; schemepp/file : string -> void
  (define (schemepp/file file)
    (with-input-from-file file schemepp))
  
  ;; schemepp/stxs : (listof syntax) -> (listof sexpr)
  (define (schemepp/stxs stxs)
    (parameterize [(current-namespace (make-initial-namespace))]
      (let* [(estxs (map/in-order expand-top-level-with-compile-time-evals stxs))
             (ers (expand/rename estxs))]
        (map syntax-object->datum ers))))

  ;; make-initial-namespace : -> namespace
  (define (make-initial-namespace)
    (let [(srcns (current-namespace))
          (newns (make-namespace))]
      (parameterize [(current-namespace newns)]
        (namespace-attach-module srcns 'mzscheme)
        (namespace-require 'mzscheme)
        (namespace-transformer-require 'mzscheme))
      newns))
  
  ;; map/in-order : (a -> b) (listof a) -> (listof b)
  (define (map/in-order f items)
    (let loop ((items items) (accum null))
      (if (null? items)
          (reverse accum)
          (loop (cdr items) (cons (f (car items)) accum)))))
  
  ;; read-all-syntaxes : -> (listof syntax)
  (define (read-all-syntaxes)
    (let loop ((forms null))
      (let [(next (read-syntax 'schemepp-input))]
        (if (eof-object? next)
            (reverse forms)
            (loop (cons next forms))))))
  
  ;; expand/rename : (listof syntax) -> (listof sexpr)
  (define (expand/rename stxs)
    (let* [(c (make-collector))
           (exps (map (convert c) stxs))]
      (append (collector-requires c)
              exps)))
  
  ;; convert : Collector -> syntax Env -> syntax
  ;; Passes: alpha conversion
  (define (convert c)
    (lambda (stx)
      (tidy (alpha stx null c))))
  
  ;; alpha-convert : syntax Env Collector -> syntax
  (define (alpha stx env c)
    (kernel-syntax-case stx #f
      [x 
       (identifier? #'x)
       (lookup-variable env c #'x)]
      [(quote x) stx]
      [(quote-syntax x) stx]
      [(#%datum . x) stx]
      [(#%top . x)
       #`(#%top . #,(intern-external-name c #'x))]
      [(#%app x ...)
       (with-syntax (((newx ...) (map (lambda (x) (alpha x env c)) (syntax-e #'(x ...)))))
         #'(#%app newx ...))]
      [(lambda formal body ...)
       (let* ((newformal (new-names/shaped  #'formal))
              (newenv (extend-env* env #'formal newformal)))
         #`(lambda #,newformal .
                   #,(map (lambda (x) (alpha x newenv c)) (syntax-e #'(body ...)))))]
      [(case-lambda (formal body ...) ...)
       (let* ((newformals (map new-names/shaped (syntax-e #'(formal ...))))
              (newenv (extend-env** env #'(formal ...) newformals)))
         (with-syntax ((((newbody ...) ...) 
                        (map (lambda (xs) (map (lambda (x) (alpha x newenv c)) (syntax-e xs)))
                             (syntax-e #'((body ...) ...))))
                       ((newformal ...) newformals))
           #'(case-lambda (newformal newbody ...) ...)))]
      [(let-values (((var ...) expr) ...) body ...)
       (let* ((newvars (map new-names/shaped (syntax-e #'((var ...) ...))))
              (newenv (extend-env** env #'((var ...) ...) newvars)))
         (with-syntax ((((newvar ...) ...) newvars)
                       ((newexpr ...) 
                        (map (lambda (x) (alpha x env c)) (syntax-e #'(expr ...))))
                       ((newbody ...) 
                        (map (lambda (x) (alpha x newenv c)) (syntax-e #'(body ...)))))
           #`(let-values (((newvar ...) newexpr) ...) newbody ...)))]
      [(letrec-values (((var ...) expr) ...) body ...)
       (let* ((newvars (map new-names/shaped (syntax-e #'((var ...) ...))))
              (newenv (extend-env** env #'((var ...) ...) newvars)))
         (with-syntax ((((newvar ...) ...) newvars)
                       ((newexpr ...) 
                        (map (lambda (x) (alpha x newenv c)) (syntax-e #'(expr ...))))
                       ((newbody ...) 
                        (map (lambda (x) (alpha x newenv c)) (syntax-e #'(body ...)))))
           #`(letrec-values (((newvar ...) newexpr) ...) newbody ...)))]
      [(begin body ...)
       (with-syntax (((newbody ...) 
                      (map (lambda (x) (alpha x env c)) (syntax-e #'(body ...)))))
         #'(begin newbody ...))]
      [(begin0 body ...)
       (with-syntax (((newbody ...) 
                      (map (lambda (x) (alpha x env c)) (syntax-e #'(body ...)))))
         #'(begin0 newbody ...))]
      [(set! var value)
       #`(set! #,(lookup-variable env c #'var) #,(alpha #'value env c))]
      [(with-continuation-mark x ...)
       (with-syntax (((newx ...) (map (lambda (x) (alpha x env c)) (syntax-e #'(x ...)))))
         #'(with-continuation-mark newx ...))]
      [(if x ...)
       (with-syntax (((newx ...) (map (lambda (x) (alpha x env c)) (syntax-e #'(x ...)))))
         #'(if newx ...))]
      [(define-values (var ...) body)
       #`(define-values #,(map (lambda (v) (intern-external-name c v)) (syntax-e #'(var ...)))
           #,(alpha #'body env c))]
      ;define-syntaxes
      [(define-syntaxes names macros)
       ;; Don't need these
       #'(begin)]
      ;module
      ;#%plain-module-begin
      [(require . _)
       ;; Leave require alone
       stx]
      ;provide
      ;require-for-syntax
      [(require-for-syntax . _)
       ;; Don't need these
       #'(begin)]
      ))
  
  ;; new-names/shaped : (syntax (implistof identifier))
  ;;                    -> (implistof identifier)
  (define (new-names/shaped stx)
    (syntax-case stx ()
      [(a . bs)
       (cons (genname #'a) (new-names/shaped #'bs))]
      [() null]
      [a (identifier? #'a) (genname #'a)]))
  
  ;; genname : identifier -> symbol
  (define (genname id)
    (gensym (string-append (symbol->string (syntax-e id)) ":")))
  
  ;; An Env is 
  ;;   - null
  ;;   - (cons (cons identifier symbol) Env)
  
  ;; extend-env : Env identifier symbol -> Env
  (define (extend-env env old new)
    (unless (identifier? old)
      (error 'extend-env "not an id: ~s~n~s" old env))
    (cons (cons old new) env))
  
  ;; extend-env* : Env (stx (implistof identifier)) (implistof symbol) -> Env
  ;; Handles both proper and improper lists of identifiers.
  (define (extend-env* env olds news)
    (cond [(stx-pair? olds)
           (extend-env (extend-env* env (stx-cdr olds) (cdr news))
                       (stx-car olds)
                       (car news))]
          [(stx-null? olds)
           env]
          [(identifier? olds)
           (extend-env env olds news)]
          [else (error 'extend-env* "got: ~s and ~s" olds news)]))
  
  ;; extend-env** : Env (stx (listof (stx (listof identifier)))) (listof (stx (listof symbol)) -> Env
  (define (extend-env** env oldss newss)
    (let loop ((env env) (oldss (syntax-e oldss)) (newss newss))
      (if (null? oldss)
          env
          (loop (extend-env* env (car oldss) (car newss))
                (cdr oldss)
                (cdr newss)))))
  
  ;; lookup-env : Env identifier -> symbol
  (define (lookup-env env id)
    (cond [(null? env)
           (error 'lookup-env "not found: ~s" (syntax-e id))]
          [(bound-identifier=? id (caar env))
           (cdar env)]
          [else (lookup-env (cdr env) id)]))
  
  ;; A Collector is
  ;;   - box of (listof (cons identifier symbol))
  
  ;; make-collector : -> Collector
  (define (make-collector) (box null))
  
  ;; lookup-external-name : (listof (cons identifier symbol)) identifier
  ;;                        -> #f|symbol
  (define (lookup-external-name cenv id)
    (cond [(null? cenv) #f]
          [(free-identifier=? id (caar cenv)) (cdar cenv)]
          [else (lookup-external-name (cdr cenv) id)]))
  
  ;; intern-external-name : Collector identifier -> symbol
  (define (intern-external-name c id)
    (cond [(not (identifier-binding id))
           ;; Not module-imported
           (syntax-e id)]
          [(free-identifier=? id (namespace-symbol->identifier (syntax-e id)))
           ;; Don't need to rename
           (syntax-e id)]
          [(lookup-external-name (unbox c) id)
           => ;; Already in table--just return
           values]
          [else
           (fprintf (current-error-port)
                    "intern-external-name: must rename ~s~n" (syntax-e id))
           (let [(sym (genname id))]
             (set-box! c (cons (cons id sym) (unbox c)))
             sym)]))
  
  ;; collector-requires : Collector -> (listof syntax)
  (define (collector-requires c)
    (map collector-make-require (unbox c)))
  
  ;; collector-make-require : (cons identifier symbol) -> syntax
  (define (collector-make-require p)
    (let* [(id (car p))
           (sym (cdr p))
           (idbinding (identifier-binding id))]
      (unless (pair? idbinding)
        (error 'collector-make-require "horribly wrong"))
      (let* [(nom-mod (caddr idbinding))
             (nom-sym (cadddr idbinding))
             (mod (if (module-path-index? nom-mod)
                      `(file ,(resolve-module-path-index nom-mod #f))
                      nom-mod))]
        (make-invasive-require mod sym nom-sym))))

  ;; make-simple-require : module-spec symbol symbol -> syntax
  ;; Bind a name to a module export.
  (define (make-simple-require mod sym export-sym)
    #`(require (rename #,mod #,sym #,export-sym)))
  
  ;; make-invasive-require : module-spec symbol symbol -> syntax
  ;; Bind a name to an unexported module variable with the given name.
  ;; Only works on non-attached modules and doesn't allow set!.
  (define (make-invasive-require mod sym export-sym)
    #`(define-syntax sym
        (make-rename-transformer
         (parameterize [(current-namespace (module->namespace '#,mod))]
           (namespace-symbol->identifier '#,export-sym)))))
  
  ;; make-horrible-require : module-spec symbol symbol -> syntax
  ;; Only works on non-attached modules. Attempts to allow set!.
  (define (make-horrible-require mod sym export-sym)
    #`(define-syntax #,sym
        (make-set!-transformer
         (lambda (stx)
           (syntax-case stx (set!)
             [(set! id v)
              #'(parameterize [(current-namespace (module->namespace '#,mod))]
                  (namespace-set-variable-value! '#,export-sym))]
             [id 
              (identifier? #'id)
              (parameterize [(current-namespace (module->namespace '#,mod))]
                (namespace-symbol->identifier '#,export-sym))])))))
  
  ;; lookup-variable : Env Collector identifier -> symbol
  (define (lookup-variable env c id)
    (if (eq? (identifier-binding id) 'lexical)
        (lookup-env env id)
        (intern-external-name c id)))
  
  ;; -------------------------------------
  
  ;; tidy : syntax -> syntax
  (define (tidy stx)
    (kernel-syntax-case stx #f
      [(#%datum . x) #'x]
      [(#%top . id) #'id]
      [(lambda formals body ...)
       (with-syntax (((newbody ...) (map tidy (syntax-e #'(body ...)))))
         #'(lambda formals newbody ...))]
      ;case-lambda
      [(let-values (((id) expr) ...) body ...)
       (with-syntax (((newexpr ...) (map tidy (syntax-e #'(expr ...))))
                     ((newbody ...) (map tidy (syntax-e #'(body ...)))))
         #'(let ((id newexpr) ...) newbody ...))]
      [(let-values ((ids expr) ...) body ...)
       (with-syntax (((newexpr ...) (map tidy (syntax-e #'(expr ...))))
                     ((newbody ...) (map tidy (syntax-e #'(body ...)))))
         #'(let-values ((ids newexpr) ...) newbody ...))]
      [(letrec-values (((id) expr) ...) body ...)
       (with-syntax (((newexpr ...) (map tidy (syntax-e #'(expr ...))))
                     ((newbody ...) (map tidy (syntax-e #'(body ...)))))
         #'(letrec ((id newexpr) ...) newbody ...))]
      [(letrec-values ((ids expr) ...) body ...)
       (with-syntax (((newexpr ...) (map tidy (syntax-e #'(expr ...))))
                     ((newbody ...) (map tidy (syntax-e #'(body ...)))))
         #'(letrec-values ((ids newexpr) ...) newbody ...))]
      [(begin x ...)
       (with-syntax (((newx ...) (map tidy (syntax-e #'(x ...)))))
         #'(begin newx ...))]
      [(begin0 x ...)
       (with-syntax (((newx ...) (map tidy (syntax-e #'(x ...)))))
         #'(begin0 newx ...))]
      [(set! var value)
       #`(set! var #,(tidy #'value))]
      ;wcm
      [(if x ...)
       (with-syntax (((newx ...) (map tidy (syntax-e #'(x ...)))))
         #'(if newx ...))]
      [(#%app x ...)
       (with-syntax (((newx ...) (map tidy (syntax-e #'(x ...)))))
         #'(newx ...))]
      [(define-values (id) body)
       #`(define id #,(tidy #'body))]
      [(define-values ids body)
       #`(define-values ids #,(tidy #'body))]
      [_ stx]))
  )

