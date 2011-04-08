(module compat-for-399 mzscheme
  (require (planet "version-case.ss" ("dyoo" "version-case.plt" 1 4)))

  (version-case
   ((version< (version) "3.99")
    (provide get-struct-info-as-list)
    (define (get-struct-info-as-list x)
      x))

   ((version< (version) "3.99.0.16")
     ;; For <3.99.0.16 (before list-immutable was reintroduced to mzscheme)
     (require scheme/struct-info)
     (provide list-immutable get-struct-info-as-list)

    (define (list-immutable . a-list) a-list)

     (define (get-struct-info-as-list a-struct-info)
       (extract-struct-info a-struct-info)))

   (else
     ;; After r8842 in svn, mzscheme has list-immutable again.
     (require scheme/struct-info)
     (provide get-struct-info-as-list)

     (define (get-struct-info-as-list a-struct-info)
      (extract-struct-info a-struct-info)))))
