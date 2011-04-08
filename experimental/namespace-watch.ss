
(module namespace-watch mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred"))
  (provide watch-namespace)
  
  (define-syntax (testers stx)
    (syntax-case stx ()
      [(_)
       #''()]
      [(_ n . ns)
       (identifier? #'n)
       (with-syntax ([p (datum->syntax-object 
                         #'here 
                         (string->symbol (format "~a?" (syntax-e #'n))))])
         #'(cons (lambda (x) (and (p x) 'n)) (testers . ns)))]
      [(_ expr . ns)
       #'(cons expr (testers ns))]))
  
  (define tag-testers
    (testers boolean number char symbol string bytes vector null port eof-object
             procedure
             box keyword syntax promise hash-table channel
             parameter namespace path regexp custodian 
             list pair))
  
  (define namespace-watcher%
    (class object%
      (init-field namespace)
      (define nsht (make-hash-table))
      (define counter 0)
      
      (define view 
        (new namespace-view% (controller this)))
      (define alarm 
        (new timer% 
             (notify-callback 
              (lambda () (when (send view is-shown?) (update))))
             (interval 2000)))
      
      (define/private (update)
        (define nslist null)
        (define change? #f)
        (for-each (lambda (s)
                    (let ([p (hash-table-get nsht s falsep)]
                          [info (get-info s)])
                      (if (and p (equal? (cdr p) info))
                          (set! nslist (cons (cons s p) nslist))
                          (begin
                            (set! nslist (cons (cons s (cons counter info)) nslist))
                            (hash-table-put! nsht s (cons counter info))
                            (set! change? #t)))))
                  (namespace-mapped-symbols namespace))
        (when change?
          (set! counter (add1 counter))
          (send view update (sort nslist))))
      
      (define/private (sort nslist)
        (quicksort nslist psymbol<?))
      
      (define/private (get-info sym)
        (parameterize ([current-namespace namespace])
          (let ([info (identifier-binding (namespace-symbol->identifier sym))]
                [type (get-type sym)])
            (cond [(eq? info #f)
                   (format "global ~a" type)]
                  [(list? info)
                   (format "imported from ~a, ~a" (mpi-fix (car info)) type)]
                  [else "(internal error)"]))))
      
      (define/private (get-type sym)
        (let/ec escape
          (parameterize ([current-namespace namespace])
            (let ([value (namespace-variable-value 
                          sym 
                          #t
                          (lambda () (escape "bound as syntax")))])
              (or (ormap (lambda (p) (p value))
                         tag-testers)
                  'unknown)))))
      
      (define/private (mpi-fix x)
        (if (module-path-index? x)
            (let-values ([(path base) (module-path-index-split x)]) path)
            x))
      
      (super-new)
      (update)))
  
  (define namespace-view%
    (class object%
      (init-field controller)
      (define frame
        (new frame%
             (label "Namespace watcher")
             (width 300)
             (height 500)))
      (define text (new text%))
      (define ecanvas (new editor-canvas% (parent frame) (editor text)))
      (send frame show #t)
      
      (define/public (is-shown?)
        (send frame is-shown?))
      
      (define/public (update nslist)
        (let ([start-b (box 0)])
          (send* text
            (begin-edit-sequence)
            (lock #f)
            (get-visible-position-range start-b #f)
            (erase))
          (for-each (lambda (p) 
                      (send* text
                        (change-style name-style)
                        (insert (format "~s" (car p)))
                        (change-style description-style)
                        (insert (format "    ~a~n" (cddr p)))))
                    nslist)
          (send* text
            (scroll-to-position (unbox start-b))
            (lock #t)
            (end-edit-sequence))))
      
      (send text lock #t)
      (super-new)))
  
  (define name-style
    (let ([sd (make-object style-delta%)])
      (send sd set-delta 'change-weight 'bold)
      (send sd set-delta-foreground "blue")
      sd))
  
  (define description-style
    (let ([sd (make-object style-delta%)])
      (send sd set-delta 'change-weight 'normal)
      (send sd set-delta-foreground "black")
      sd))
  
  (define (falsep) #f)

  (define (psymbol<? a b)
    (or (< (cadr a) (cadr b))
        (and (= (cadr a) (cadr b))
             (string<? (symbol->string (car a)) (symbol->string (car b))))))
  
  (define (watch-namespace ns)
    (new namespace-watcher% (namespace ns)))
  )
