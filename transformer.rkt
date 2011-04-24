#lang racket/base
(require syntax/stx
         (for-template racket/base))
(provide id->expr-transformer)

(define (id->expr-transformer expr)
  (let ([expr #`(#%expression #,expr)])
    (lambda (stx)
      (cond [(stx-pair? stx)
             (datum->syntax stx
                            (cons expr (stx-cdr stx)))]
            [(identifier? stx)
             expr]
            [else
             (raise-syntax-error #f "bad syntax" stx)]))))
