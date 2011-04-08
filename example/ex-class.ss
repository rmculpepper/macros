
(module ex-class mzscheme
  (require "../class-iop.ss"
           (lib "class.ss"))
  
  (define-interface stack<%> (empty? push pop))
  (define s%
    (class* object% (stack<%>)
      (define items null)
      (define/public (empty?)
        (null? items))
      (define/public (push item)
        (set! items (cons item items)))
      (define/public (pop)
        (begin0 (car items)
                (set! items (cdr items))))
      (public add-all)
      (define: (add-all (s : stack<%>))
        (let loop ()
          (unless (send: s stack<%> empty?)
            (push (send: s stack<%> pop))
            (loop))))
      (super-new)))
  
  (define: s1 stack<%> (new s%))
  (define: s2 stack<%> (new s%))
  
  (send: s1 stack<%> push 'a)
  (send: s1 stack<%> push 'b)
  (send s2 add-all s1)
  
  ;; Disabled for now...
  ;(display (s2.pop)) (newline)
  ;(display (s2.pop)) (newline)
  
  #;(define stackif stack<%>)
  #;(send: s1 stackif empty?)
  )
