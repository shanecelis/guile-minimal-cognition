(define-module (infix)
  #:export-syntax (:))

;; Very cool little infix macro by Anton van Straaten.

;;https://groups.google.com/forum/m/?fromgroups#!msg/comp.lang.scheme/wCHiLwh1Iyo/KcsxxgIS8UcJ

(define-syntax :
  (syntax-rules (@ := begin)
    ((: (begin . args))
      (begin . args))
    ((: v @ i)
     (array-ref v (: i)))
    ((: v @ i := rhs ...)
     (array-set! v (: rhs ...) (: i)))
    
    ;; ((: (op x ...))
    ;;  (op (: x ...)))
    ((: (op x))
     (op (: x)))
    
    ;; ((: op (x ...) next-op ...)
    ;;  (let ((z (op (: x ...))))
    ;;    (: z next-op ...)))


    ((: (op x) next-op ...)
     (let ((z (op (: x))))
       (: z next-op ...)))
    
    ((: w prev-op (op x) next-op ...)
     (let ((y (op (: x))))
       (: w prev-op y next-op ...)))
    
    ((: x op y)
     (op (: x) (: y)))
    ((: x op v @ i next-op ...)
     (let ((y (vector-ref v (: i))))
       (: x op y next-op ...)))
    ((: x op y next-op ...)
     (let ((z (: x op y)))
       (: z next-op ...)))
    ((: (x ...))
     (: x ...))
    ((: x) x)))
