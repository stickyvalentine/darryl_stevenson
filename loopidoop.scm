(load "list-man.scm")
(load "math.scm")

(define (dothis l)
  (if (and (> (lslen? l) 1) (in_list? (car l) '("+" "-")))
      (cond ((equal? (car l) "+") (do_add+ (cdr l)))
	    ((equal? (car l) "-") (do_sub+ (cdr l)))
	    )
      (print l))
  (darryl))

(define (darryl)
  (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))


(define (do_int_eater l)
  (if (and (> (lslen? l) 2) (integer? (car l)))
      (do_int_eater (cdr l))
      (dothis l)))

(define (in_list? i l)
  (if (null? l)
      #f
      (or (equal? (car l) i) (in_list? i (cdr l)))))

(define (add+ l)
  (if (and (>1 l) (integer? (car l)))
      (add+ (replace-nth (cdr l)
			 (+ -1 (lslen? (cdr l)))
			 (+ (car l) (tail l))))
      (dothis l)))

(define (sub+ l)
  (if (and (>1 l) (integer? (car l)))
      (sub+ (replace-nth (cdr l)
			 (+ -1 (lslen? (cdr l)))
			 (- (tail l) (car l))))
      (dothis l)))

(print "------------------------------")
(print "Darryl Stevenson - 1st Attempt")
(print "------------------------------")
(darryl)
