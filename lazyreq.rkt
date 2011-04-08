#lang racket/base
(require racket/runtime-path
         racket/promise)
(provide lazy-require-functions
         lazy-require-variables)

;; require triggered on *application*

(define-syntax (lazy-require-functions stx)
  (syntax-case stx ()
    [(_ modpath fun ...)
     (begin
       (for ([fun (syntax->list #'(fun ...))])
         (unless (identifier? fun)
           (raise-syntax-error #f "expected identifier for function name" stx fun)))
       #'(begin (define-runtime-module-path-index mpi modpath)
                (lazyfun mpi fun) ...))]))

(define-syntax-rule (lazyfun mpi fun)
  (begin (define fun-p (delay (dynamic-require mpi 'fun)))
         (define fun
           (rename-procedure
            (make-keyword-procedure
             (lambda (kws kwargs . args)
               (keyword-apply (force fun-p) kws kwargs args)))
            'fun))))

;; ----

;; require triggered on *variable reference*
;; so can't lazy-require, re-provide with contract w/o triggering require,
;; since provide/contract counts as a reference (I think)

(define-syntax (lazy-require-variables stx)
  (syntax-case stx ()
    [(_ modpath var ...)
     (begin
       (for ([var (syntax->list #'(var ...))])
         (unless (identifier? var)
           (raise-syntax-error #f "expected identifier for variable name" stx var)))
       #'(begin (define-runtime-module-path-index mpi modpath)
                (lazything mpi var) ...))]))

(define-syntax-rule (lazything mpi var)
  (begin (define var-p (delay (dynamic-require mpi 'var)))
         (define-syntax var
           (id->expr-transformer #'(force var-p)))))
