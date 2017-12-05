(define target 289326)

;this will hold pairs . the first part of the pair is a list of the coords
;the second the value
(define pknown '())

(define (store x y val) ; append to the points known list
  (set! pknown (cons (cons (list x y) val)  pknown)))

(define (printpoints lst)
  (if (not (null? lst))
    (begin
      (format #t "x:~D y:~D v:~D ~%" (caar lst) (cadar lst) (cdar lst))
      (printpoints (cdr lst)))))

(define xstep 1)

         
(define (getvalcoord cc) ;look for the value those coordinates and return 0 if not stored.
  (let ((point (assoc cc pknown)))
    (if (not point)
        0
        (cdr point))))

(define (spiralsnake)
  (let* ((atx 0)
        (aty 0)
        (steps 1)
	(curstep 0)
        (xwalk? #t) ;means walking along the x axis. we use this to flip the ops every 2 dimension changes
        (curval 1)
        (getvalneighbors (lambda () ;looks on the pknown list for neighbors at part 2 and sum the vals
          (let ((n (list atx (+ aty 1)))
                (e (list (+ atx 1) aty)) 
                (s (list atx (- aty 1)))
                (w (list (- atx 1) aty))
                (ne (list (+ atx 1) (+ aty 1))) 
                (se (list (+ atx 1) (- aty 1)))
                (nw (list (- atx 1) (+ aty 1)))
                (sw (list (- atx 1) (- aty 1))))
          (+ (getvalcoord n) (getvalcoord e) (getvalcoord s) (getvalcoord w)
             (getvalcoord ne) (getvalcoord se) (getvalcoord nw) (getvalcoord sw)))))
            
        (getvalsimple (lambda () ; this is for part 1
          (let ((toret curval))
            (set! curval (+ 1 curval))
            toret)))
        (op +); initially the snake increases coords
        (flipop (lambda ();flips the operation on the direction the snake is wakling
                 (if (eq? op +)
                       (set! op -)
                       (set! op +))))
        (walk (lambda ()
          (if  (>= curstep steps) 
            (if xwalk? 
                (begin (set! xwalk? #f) (set! curstep 1))
                (begin (set! xwalk? #t) (set! steps (+ 1 steps)) (set! curstep 1) (flipop)))
            (set! curstep (+ 1 curstep)))
          (if xwalk?
              (set! atx (op atx 1))
              (set! aty (op aty 1)))
          (format #t "at step:~D from total steps ~D heading x:~A (~D,~D)~%" curstep steps xwalk? atx aty)
          (store atx aty (getvalneighbors))
	  )))
    (store 0 0 1);set the initial val at 0 0
    walk))

(define (walkToTarget targ); invoke the snake with a target input.
  (letrec ((mysnake (spiralsnake))
        (solve (lambda ()
           (mysnake)
           (if (> (cdar pknown) targ)
               (car pknown)
               (solve)))))
    (solve)))
    

