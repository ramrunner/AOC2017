(use extras)

;;read input turn it into a list of numbers
(define readlist
  (lambda (ln acc)
   (if (null? ln) acc (cons (string->number ln) acc))))

(define solveJump
  (lambda (fname)
   (letrec* ((jlist (list->vector (foldr readlist '() (read-lines fname)))) ;treat it as a mutable vector
         (step 0)
         (jlength (vector-length jlist))
         (jumper (lambda (ind) ;set up a recursive lambda that takes the current index on the vec
           (let ((localstore 0))
             (if (or (>= ind jlength) (< ind 0)) ;jumps out of the boundaries
                step
                (begin
                  (set! localstore (vector-ref jlist ind))
		  (set! step (+ 1 step))
                  (if (>= localstore 3) ; for part one just do the else clause
                    (begin (vector-set! jlist ind (- localstore 1)))
                    (begin (vector-set! jlist ind (+ 1 localstore))))
                  (jumper (+ ind localstore))))))))
     (format #t "solved in:~A~%" (jumper 0)))))

(solveJump "input1day5")
