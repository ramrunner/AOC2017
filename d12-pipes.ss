(use srfi-13)
(use format)
(use vector-lib)

;;implemented brute connected components. each index of the array
;;reference the parent. upon a connection all the relevant parents
;;change to the updated one
(define rel (make-vector 1))
(vector-set! rel 0 0)
(define (readline ln)
  (letrec* ((parts (string-split ln " <-> ,"))
         (root (string->number (car parts)))
         (nodes (map string->number (cdr parts)))
         (maxind (apply max (cons root nodes)))
         (chroot (lambda (ind root) 
                  ;(format #t "recur with ~A ~A~%" ind root)
                  (letrec ((oldroot (vector-ref rel ind))
                        (recur (lambda (ind oldroot newroot)
                          (if (< ind (vector-length rel))
                            (begin
                              (if (eq? (vector-ref rel ind) oldroot)
                                 (vector-set! rel ind newroot))
                              (recur (+ 1 ind) oldroot newroot))))))
                    (recur 0 oldroot root))))
         (inite (lambda (from to)
                (if (<= from to)
                  (begin
                    (vector-set! rel from from)
                    (inite (+ from 1) to))))))
    (if (> maxind (vector-length rel))
        (let ((curl (vector-length rel)))
          (set! rel (vector-copy rel 0  (+ 1 maxind) 0))
          (inite (- curl 1) maxind)))
    (map (lambda (e) (chroot e root)) nodes)))

;;first part
(fold (lambda (e acc) (if (eq? e (vector-ref rel 0)) (+ 1 acc) acc)) 0 (vector->list rel))


;;second part
;;copied from SO . counts uniq elems in list.
(define (uniquely list)
  (let looking ((rslt '()) (list list))
    (if (null? list)
        rslt
        (let ((next (car list))
              (rest (cdr list)))
          (if (list? next)
              (looking rslt (append next rest))
              (looking (if (memq next rslt)
                           rslt
                           (cons next rslt))
                       rest))))))

(length (uniquely (vector->list rel)))
