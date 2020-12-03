(import srfi-69)
(import format)

(define (!= a b)
  (not (eq? a b)))

(define (== a b)
  (eq? a b))

(define (runvm file)
 (let* ((registers (make-hash-table))
       (maxval 0)
       (maxeverval 0)
       (logmax (lambda (v) (if (> v maxeverval) (begin (set! maxeverval v) v) v))))
  (letrec-syntax 
    ((vm 
      (syntax-rules ()
       ((_ reg rop hm ifs a1 op a2)
        (cond ((if a1 op a2)
                   (cond ((hash-table-exists? registers 'reg)
                           (let ((cval (hash-table-ref registers 'reg)))
                             (hash-table-set! registers 'reg (logmax (+ cval (rop hm))))))
                         (else (hash-table-set! registers 'reg (logmax (+ 0 (rop hm)))))))
              (else (format #t "no vm action on this line ~%"))))))
     (if
      (syntax-rules ()
        ((_ arg1 op arg2)
         (cond  ((hash-table-exists? registers 'arg1) (cond ((op (hash-table-ref registers 'arg1) arg2) #t) (else #f)))
                (else (cond ((op 0 arg2)  #t)
                            (else #f)))))))
     (inc 
      (syntax-rules ()
        ((inc by)
         (begin (format #t "matched inc~%") (+ 0 by)))))
     (dec 
      (syntax-rules ()
        ((dec by)
         (begin (format #t "matched dec~%") (- 0 by))))))

(vm jaq inc 89 if nm > -2345)
(vm ntk dec -524 if vv >= -1669)
(vm ntk dec 371 if s < 897)
(vm s dec -478 if jaq > -4170)
(vm j inc 924 if ipq == -1184)
(vm ntk inc -239 if y == -846)
(vm wby inc -397 if ntk >= -2941)
(vm nm dec 907 if qen == 2233)
(format #t "registers: ~A ~% ~%" (hash-table->alist registers))
     (map (lambda (p) (cond ((> (cdr p) maxval) (set! maxval (cdr p))))) (hash-table->alist registers))
     (format #t "max is :~A max ever:~A~%" maxval maxeverval))))

(runvm "inputs/day8")

