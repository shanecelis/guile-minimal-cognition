(define-module (vector-math)
  #:use-module (ice-9 optargs)
  #:use-module (debugging assert)
  #:use-module (srfi  srfi-1)
  #:use-module (oop goops)
  #:use-module ((rnrs) #:select (vector-map vector-for-each))
  #:export (vector-fold
            vector-fold1
            vector-sum
            vector-every
            vector-any
            vector-norm
            vector-normalize
            vector-abs
            vector-min
            vector-max
            vector-bound
            vector+
            vector-
            vector*
            vector.
            vector-negate
            vector-move!
            vector-move!*
            vector-angle
            min-of-vector
            max-of-vector
            range
            index-range
            make-matrix
            matrix.
            matrix*
            matrix-ref
            matrix-set!
            make-identity-matrix
            make-translation-matrix
            make-rotation-matrix
            make-rotation-matrix-euler-y
            pi
            =?
            vector=?
            matrix-copy
            list-transpose
            matrix-transpose
            matrix->list
            list->matrix
            solve-line-intersect-2d
            list-op
;            vector-map!
            vector-take
            vector-drop
            vector-append
            vector->string
            vector->matrix
            vector-rotate-right
            vector-rotate-left
            linear->subscript
            linear->subscript*
            subscript->linear
            subscript->linear*
            matrix?
            )
  #:export-syntax ())


;; XXX should return a vector?

;; XXX possible convention:
;; fold-vector should not return a vector.
;; vector-fold should.
(define (vector-fold f init v)
  (fold f init (array->list v)))

(define (vector-fold1 f v)
  (let ((lst (array->list v)))
   (fold f (car lst) (cdr lst))))

(define (vector-abs v)
  (vector-map abs v))


(define (vector-every f v)
  (vector-fold (lambda (a b) (and a b))
               #t (vector-map f v)))

(define (vector-any f v)
  (vector-fold (lambda (a b) (or a b))
               #f (vector-map f v)))

(define (vector-sum v)
  (vector-fold + 0 v))

(define (vector-mean v)
  (/ (vector-sum v) (vector-length v)))

(define (vector-norm-squared v)
  (let ((sv (vector-map * v v)))
            (vector-fold + 0 sv)))

(define (vector-norm v)
  (sqrt (vector-norm-squared v)))

(define (vector-angle v w)
  (let ((dp (vector. v w))
        (vn (vector-norm v))
        (wn (vector-norm w)))
    (acos (/ dp (* vn wn)))))

(define (vector-normalize v)
  (vector* (/ 1 (vector-norm v)) v))

(define (vector. a b)
  (vector-fold + 0 (vector-map * a b)))

(define (vector-min a b)
  (scalar-vector-fn min a b))

(define (min-of-vector v)
  (vector-fold1 min v))

(define (max-of-vector v)
  (vector-fold1 max v))

(define (vector-max a b)
  (scalar-vector-fn max a b))

(define (vector-bound a b v)
  "Bound the vector v between [a, b]."
  (vector-min b (vector-max a v)))

(define (scalar-vector-fn fn a b)
  (if (vector? a)
      (vector-map fn a b)
      (vector-map (lambda (x) (fn a x)) b)))

(define (vector+ a b)
  (scalar-vector-fn + a b))

;; (define (vector+ a b)
;;   (if (vector? a)
;;       (vector-map + a b)
;;       (vector-map (lambda (x) (+ a x)) b)))

(define (vector* a b)
  (scalar-vector-fn * a b))

(define (vector-negate v)
  (vector* -1 v))

(define (vector- a b)
  (vector+ a (vector-negate b)))

(define (vector. a b)
  (vector-fold + 0 (vector* a b)))

(define* (range m n #:optional (inc 1))
  (define (range* m n inc comp) 
    (if (comp m n)
        '()
        (cons m (range* (+ m inc) n inc comp))))
  (if (and (> m n) (> inc 0))
      (range* m n (- inc) <)
      (range* m n inc >)))

(define (index-range v)
  (range 0 (1- (vector-length v))))

(define* (vector-range m n #:optional (inc 1))
  (list->vector (range m n inc)))

(define (vector-move! dest src)
  "Copy src to dest."
  (for-each (lambda (i)
              (vector-set! dest i (vector-ref src i))
              ) (range 0 (1- (vector-length dest)))))

(define (vector-move!* dest di df src si sf)
  (if (not (= (- sf si) (- df di)))
      (throw 'different-ranges))
  (for-each (lambda (i)
              (generalized-vector-set! dest (+ di i) (generalized-vector-ref src (+ si i))))
            (range 0 (- sf si)))
  dest)

(define* (make-matrix m #:optional (n m) (fill 0))
  (vector-map (lambda (i) (make-vector n fill)) (vector-range 0 (1- m))))

(define (vector->matrix v m n)
  (assert (= (vector-length v) (* m n)))
  (let ((M (make-matrix m n)))
    ;; row-major order
    (for-each (lambda (k)
                (let ((i (quotient  k n))
                      (j (remainder k n)))
                    (matrix-set! M i j (vector-ref v k)))) 
              (range 0 (1- (vector-length v))))
    M))

(define (list-transpose M)
  (if (null? (car M))
      '()
      (cons (map car M) (list-transpose (map cdr M)))))

(define (matrix->list M)
  (map vector->list (vector->list M)))

(define (list->matrix M)
  (vector-map list->vector (list->vector M)))

(define (list-matrix-op fn . matrices)
  (list->matrix (apply fn (map matrix->list matrices))))

(define (matrix-transpose M)
  (list-matrix-op list-transpose M))

(define (matrix. m . args)
  (if (null? args)
      m
   (let ((v (car args)))
     (if (matrix? v) 
         (let ((N (matrix-transpose v)))
           (apply matrix. 
                  (matrix-transpose (vector-map (lambda (col) (matrix. m col)) N))
                  (cdr args)))
         (if (vector? v)
          (apply matrix. (vector-map (lambda (row) (vector. row v)) m) (cdr args))
          (error "can't multiple matrices yet"))))))

(define (matrix-map fn M)
  (vector-map (lambda (row) (vector-map fn row)) M))

(define (matrix* s M)
  (matrix-map (lambda (x) (* s x)) M))

(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j))

(define (matrix-set! m i j value)
  (vector-set! (vector-ref m i) j value))

(define (make-identity-matrix m)
  (let ((M (make-matrix m m)))
    (for-each (lambda (i) (matrix-set! M i i 1)) (range 0 (1- m)))
    M))

(define (make-translation-matrix t)
  (let ((M (make-identity-matrix 4)))
    (for-each (lambda (i) (matrix-set! M i 3 (vector-ref t i))) (range 0 2))
    M))

(define (make-rotation-matrix axis rad)
  #f)

;; (define (make-cross-product-matrix vector)
;;   (let ((M (make-matrix 3 3)))
;;     (set-matrix! M 0 1 (- (vector-ref )))
;;     )
;;   )

(define pi (acos -1))

(define-method (=? (a <number>) (b <number>) . rest)
  (let-optional rest ((tolerance 0.001))
   (< (abs (- a b)) tolerance)))

(define-method (=? (a <list>) (b <list>) . rest)
  (fold (lambda (x y prev)
          (and prev (apply =? x y rest))) #t a b))

(define-method (=? (a <pair>) (b <pair>) . rest)
  (and (apply =? (car a) (car b) rest)
       (apply =? (cdr a) (cdr b) rest)))


(define-method (=? (a <vector>) (b <vector>) . rest)
  (apply =? (vector->list a) (vector->list b) rest))

(define-method (=? (a <uvec>) (b <uvec>) . rest)
  (apply =? (generalized-vector->list a) (generalized-vector->list b) rest))

(define-method (=? a b . rest)
  #f)


(define (vector=? a b)
  (catch 'not-equal
    (lambda ()  
      (vector-for-each (lambda (x y)
                         (if (not (=? x y))
                             (throw 'not-equal))) a b)
            #t)
    (lambda (key . args)
      #f)))

(define (make-rotation-matrix-euler-y theta)
  (let ((s (sin theta))
        (c (cos theta)))
    `#(#(,c 0 ,s 0)
       #(0 1 0 0)
       #(,(- s) 0 ,c 0)
       #(0 0 0 1))))

(define (matrix-copy M)
  (vector-map vector-copy M))

(define (determinant-2d M)
  (- (* (matrix-ref M 0 0)
        (matrix-ref M 1 1))
     (* (matrix-ref M 0 1)
        (matrix-ref M 1 0))))

(define (inverse-2d M)
  (let ((N (matrix-copy M))
        (det (determinant-2d M))
        )
    (if (not (=? det 0))
        (begin (matrix-set! N 0 0 (matrix-ref M 1 1))
               (matrix-set! N 1 1 (matrix-ref M 0 0))
               (matrix-set! N 0 1 (- (matrix-ref M 0 1)))
               (matrix-set! N 1 0 (- (matrix-ref M 1 0)))
               (matrix* (/ 1 det) N))
        #f)))

(define (solve-linear-2d M b)
  (matrix. (inverse-2d M) b))

(define (transpose-2d M)
  (let ((N (matrix-copy M)))
    (matrix-set! N 0 1 (matrix-ref M 0 1))
    (matrix-set! N 1 0 (matrix-ref M 1 0))
    N))

(define (solve-line-intersect-2d r1 v1 r2 v2)
  (let* ((M `#(,v1 ,(vector-negate v2)))
         (st (solve-linear-2d (transpose-2d M) (vector- r2 r1)))
         (s (if st (vector-ref st 0)))
         (t (if st (vector-ref st 1))))
    ;; (list (vector+ r1 (vector* s v1))
    ;;       (vector+ r2 (vector* t v2)))
    (if st
        (vector+ r1 (vector* s v1))
        #f)))

(define (list-op fn . vectors)
  (list->vector (apply fn (map vector->list vectors))))

(define (vector-take v n)
  (list-op (lambda (lst) (take lst n)) v))

(define (vector-drop v n)
  (list-op (lambda (lst) (drop lst n)) v))

(define (vector-append . vectors)
  (apply list-op append vectors))

(define (vector-rotate-left v n)
  (vector-append (vector-drop v n) (vector-take v n)))

(define (vector-rotate-right v n)
  (let ((len (vector-length v)))
   (vector-append (vector-drop v (- len n)) (vector-take v (- len n)))))

(define* (vector->string v #:optional (element-format "~1,2f "))
  (call-with-output-string
                    (lambda (port)
                      (format port "#(")
;                      (error #t "I'm testing this thing ~a" 'ok )
                      ;(error "I'm testing this thing ~a" 'ok )
                      (vector-for-each (lambda (x)
                                         (format port element-format x)) v)
                      (format port ")"))))

(define (linear->subscript matrix-size index)
  "Row major linear index to subscript indices."
  (reverse (linear->subscript* matrix-size index)))

(define (linear->subscript* matrix-size index)
  "Column major linear index to subscript indices (used by MATLAB and Fortran)."
  (if (null? matrix-size)
      '()
      (let* ((i (quotient index (car matrix-size)))
             (j (remainder index (car matrix-size))))
        (cons j (linear->subscript* (cdr matrix-size) i)))))

(define (subscript->linear matrix-size indices)
  "Row major subscript indices to linear index."
  (define (but-last lst)
    (cdr (reverse lst)))
  (if (= 1 (length matrix-size))
      (car indices)
      (let ((multiplier (apply * (cdr matrix-size)))) 
        (+ (* multiplier (car indices)) 
           (subscript->linear (cdr matrix-size) (cdr indices))))))

(define (subscript->linear* matrix-size indices)
  "Column major subscript indices to linear index."
  (subscript->linear (reverse matrix-size) (reverse indices)))

(define (matrix? M)
  (and (vector? M) (vector? (vector-ref M 0))))
