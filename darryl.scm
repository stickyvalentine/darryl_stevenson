(load "list-man.scm")

(define (dothis l)
  (cond ((equal? (car l) "+")
	 (addall (cdr l))))
  (cond ((equal? (car l) "-")
	 (suball (cdr l))))
  (cond ((equal? (car l) '*)
	 (multall (cdr l))))
  (cond ((equal? (car l) '/)
	 (divall (cdr l))))
  (cond ((equal? (car l) 'double)
	 (double (cdr l))))
  )

(define (valid? s)
  (if (member s '("+" - * /))
      #t
      #f))

(define (darryl)
  (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))
	
(define (darryl2)
  (loopidoop (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))

(define (addall l)
  (if (and (>1 l) (integer? (car l)))
      (addall (addone l))
      l))

(define (addone l)	  
  (replace-nth (cdr l)
	       (+ -1 (lslen? (cdr l)))
	       (+ (car l) (tail l)))
  )

(define (loopidoop l)
  (cond ((not (valid? (car l))) (print (car l) "is not a valid symbol")))
  (cond ((not (>1 l)) (l)))
  (cond (#t (loopidoop (dothis l))))
  )


(darryl)
