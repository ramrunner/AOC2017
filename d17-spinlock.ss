(import srfi-1)
(import vector-lib)
(import format)
;; for step 1.
#|
(define (add-at elem pos lst)
  (vector-unfold (lambda (i)
                   (cond ((< i pos) (vector-ref lst i))
                         ((= i pos) elem)
                         (else (vector-ref lst (- i 1)))))
                 (+ 1 (vector-length lst))))
|#
(define (add-at elem pos lst)
  (vector-set! lst pos elem))
                   
;;  (cond ((null? lst)
;;           (list elem))
;;        ((= pos 0) (cons elem lst))
;;        (else
;;          (cons (car lst)
;;                (add-at elem (- pos 1) (cdr lst))))))
       

(define (make-spin step)
  (let* ((store (make-vector 50000000 0))
         (size 1)
         (curpos 0)
         (step step)
         (go (lambda ()
               (let* ((pos (+ curpos step))
                      (actpos  (+ 1  (modulo pos size))))
                 (add-at size actpos store)
                 (set! curpos actpos)
                 (set! size (+ size 1)))))
         (print-list (lambda ()
                       (format #t "size:~A curpos:~A step:~A elem@pos 2:~A at 1:~A" size curpos step (vector-ref store 1) (vector-ref store 0)))) 
         (dispatch (lambda (m)
                     (cond ((eq? m 'go) go)
                           ((eq? m 'print) print-list)))))
    dispatch))

(define test (make-spin 304))
(define (loop cur num)
  (if (< cur num)
      (begin
       ((test 'go))
       (loop (+ 1 cur) num))))

(loop 0 50000000)
;;(loop 0 2017)
((test 'print))
