;; run like (parseline "fname")
(import format)
(import (chicken io))

(define (parseline file)
  (letrec*  ((input (string->list (car (read-lines file))))
          (inlen (length input))
          (inrubbish #f)
          (negate #f)
          (groupscore 0)
          (ignore #f)
          (totcanceled 0)
          (stack '())
          (parser (lambda (pos)
                    (if (>= pos inlen)
                      '()
                      (let ((tok (list-ref input pos)))
                        (begin 
                          (if (not inrubbish)
                            (format #t "pos:~A tok:~A ~%" pos (list-ref input pos)))
                          (if (and negate inrubbish)
                              (set! negate #f)
                              (begin
                              (case tok 
                                    ((#\{) 
                                     (if (not inrubbish)
                                     (begin
                                       (set! stack (cons (+ 1 (length stack)) stack))
                                       (format #t "gstart. stack:~A ~%" stack))))
                                    ((#\<)
                                     (if (not inrubbish)
                                     (begin
                                       (set! inrubbish #t)
                                       (set! ignore #t)
                                       (format #t "rubbish start~%"))))
                                    ((#\!)
                                     (if inrubbish
                                     (begin
                                       (set! negate #t)
                                       (format #t "negate the next~%"))))
                                    ((#\>)
                                     (begin
                                       (set! inrubbish #f)
                                       (format #t "rubbish closing ~%")))
                                    ((#\})
                                     (if (not inrubbish)
                                     (begin
                                       (set! groupscore (+ groupscore (car stack)))
                                       (format #t "gend stack:~A~%" stack)
                                       (set! stack (cdr stack))))))
                                    (if (and inrubbish (not (eq? tok #\!)) (not ignore))
                                         (set! totcanceled (+ 1 totcanceled)))
                                     (set! ignore #f))))))))
            (parserrec (lambda (ind)
                         (if (< ind inlen)
                             (begin
                               (parser ind)
                               (parserrec (+ 1 ind)))))))
    (parserrec 0)
    (format #t "Score:~A cancelled:~A~%" groupscore totcanceled)))

(call-with-input-file "inputs/day9" parseline)
