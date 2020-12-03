(import (chicken string))
(import (chicken sort))
(import format)

(define filterokpass (lambda (lines)
  (let* ((valid 0) 
         (string2slist (lambda (str) ; we solve anagram equality if we sort the list and check for equality on that.
            (sort (string->list str) char<?)))
        (filterline (lambda (ln)
                      (let ((known '())
                             (fails? #f))
                        (map (lambda (w) (if (eq? (assoc (string2slist w) known) #f)
                                            (set! known (append known (list (cons (string2slist w) #t))))
                                            (set! fails? #t))) (string-split ln))
                        (if fails? 
                            (format #t "line fails?:~A. num of words ~A words:~A :~%" fails? (length known) known)
                             (set! valid (+ 1 valid)))))))
   (map filterline lines)
   valid)))
