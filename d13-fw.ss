(use format)
(use srfi-1)

(define fw '())
(define readinput (lambda (ln)
 (let* ((pair (string-split ln ": "))
        (layer (string->number (car pair)))
        (depth (string->number (cadr pair))))
  (set! fw (cons (list layer depth (- (* 2 depth) 2)) fw)))))

(define solve (lambda()
   (letrec* ((runacc (lambda (e acc) (if (eq? (modulo (car e)  (caddr e)) 0) (+ acc (* (car e) (cadr e))) acc)))
          (runwithdlay (lambda (del) (lambda (e acc) (if (eq? (modulo (+ (car e) del)  (caddr e)) 0) (+ 1 acc (* (car e) (cadr e))) acc))))
          (firstscore (format #f "first score:~A~%" (foldr runacc 0 fw)))
          (incdlay (lambda (d) (let ((score (foldr (runwithdlay d) 0 fw))) (if (eq? score 0) d (incdlay (+ d 1)))))))
   (format #t "done with delay ~A" (incdlay 0)))))

(map readinput (read-lines "input1day13"))
(solve)
