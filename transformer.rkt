#lang racket/base
(require syntax/stx)
(provide id->expr-transformer)

(define (id->expr-transformer expr)
  (lambda (stx)
    (cond [(stx-pair? stx)
           (datum->syntax stx
                          (cons expr (stx-cdr stx)))]
          [(identifier? stx)
           expr]
          [else
           (raise-syntax-error #f "bad syntax" stx)])))
