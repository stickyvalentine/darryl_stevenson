(load "list-man.scm")
(load "math.scm")

(define (dothis l)
  (if (and (> (lslen? l) 1) (in_list? (car l) '("+" "-" "quit" "say")))
      (cond ((equal? (car l) "+") (do_add+ (cdr l)))
	    ((equal? (car l) "-") (do_sub+ (cdr l)))
	    ((equal? (car l) "say" (say (cdr l))))
	    ((equal? (car l) "quit") (exit))	    
	    )
      (print l))
  (darryl))

(define (darryl)
  (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))

(define (say l)
  (print (car l))
  (dothis (cdr l)))

(print "------------------------------")
(print "Darryl Stevenson - 1st Attempt")
(print "------------------------------")
(darryl)

