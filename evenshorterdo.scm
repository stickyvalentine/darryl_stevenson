(load "list-man.scm")
(load "math.scm")

(define (doers l)
  (cond ((equal? (car l) "say") (say (cdr l)))
	(else l)))

(define (doo l)
  (cond ((inlist? (car l) '("say")) (doers l))
	(else l)))
			
(define (darryl)
  (print (doo (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))
  (darryl))

(define (say l)
  (if (print (car l))
      (cdr l)
      (cdr l)))

(print "------------------------------")
(print "Darryl Stevenson - 1st Attempt")
(print "------------------------------")
;;;(darryl)

