
;; Quick and Dirty

(module qd mzscheme
  (require-for-syntax (lib "kerncase.ss" "syntax"))
  (provide syntax-local-value/quote
           syntax-local-value/quote-syntax
           phase1-eval/quote
           phase1-eval/quote-syntax
	   expand/stop
           expand/stop-kernel)
  
  (define-syntax (syntax-local-value/k stx)
    (syntax-case stx ()
      [(_ called-as name k)
       (unless (identifier? #'name)
         (raise-syntax-error (syntax-e #'called-as)
                             "expected identifier"
                             #'name))
       (let ([value (syntax-local-value #'name)])
         #`(k #,value))]))

  (define-syntax (syntax-local-value/quote-syntax stx)
    (syntax-case stx ()
      [(slv/qs name)
       #'(syntax-local-value/k slv/qs name quote-syntax)]))

  (define-syntax (syntax-local-value/quote stx)
    (syntax-case stx ()
      [(slv/q name)
       #'(syntax-local-value/k slv/q name quote)]))

  (define-syntax (phase1-eval/k stx)
    (syntax-case stx ()
      [(_ called-as expr k)
       #'(let-syntax ([tmp (lambda (stx) #`(k #,expr))])
           (tmp))]))

  (define-syntax (phase1-eval/quote-syntax stx)
    (syntax-case stx ()
      [(p1e/qs expr)
       #'(phase1-eval/k #'p1e/qs expr quote-syntax)]))
  
  (define-syntax (phase1-eval/quote stx)
    (syntax-case stx ()
      [(p1e/q expr)
       #'(phase1-eval/k #'p1e/q expr quote)]))

  (define-syntax (expand/stop/k stx)
    (syntax-case stx ()
      [(expand/stop/k form (stop ...) k)
       (with-syntax ([result
		      (local-expand #'form
				    (syntax-local-context)
				    (syntax->list #'(stop ...)))])
         #'(k result))]))
  
  (define-syntax (expand/stop stx)
    (syntax-case stx ()
      [(expand/stop form (stop ...))
       #'(expand/stop/k form (stop ...) quote-syntax)]))
  
  (define-syntax (expand/stop-kernel stx)
    (syntax-case stx ()
      [(expand/stop form)
       #'(expand/stop-kernel form ())]
      [(expand/stop-kernel form (additional ...))
       (with-syntax ([stops (append (syntax->list #'(additional ...))
                                   (kernel-form-identifier-list #'here))])
         #'(expand/stop/k form stops quote-syntax))]))
  
  )
