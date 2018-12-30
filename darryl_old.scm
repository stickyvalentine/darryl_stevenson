(load "list-man.scm")
(load "math.scm")

(define (dothis l)
  (if (and (> (lslen? l) 1) (in_list? (car l) '("+" "-" "*" "/" "quit" "say" "append"))) 
      (cond ((equal? (car l) "+") (do_add+ (cdr l)))
	    ((equal? (car l) "-") (do_sub+ (cdr l)))
	    ((equal? (car l) "*") (do_mul+ (cdr l)))
	    ((equal? (car l) "/") (do_div+ (cdr l)))	    
	    ((equal? (car l) "quit") (exit))
	    ((equal? (car l) "say" (do_say (cdr l))))
	    ((equal? (car l) "append" (do_append (cdr l))))	    	    
	    )
      (print l))
  (darryl))

(define (darryl)
  (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))

(define (do_say l)
  (print (car l))
  (dothis (cdr l)))

(define (do_append2 l)
  (dothis (apnd (cdr l) (car l))))

(define (do_append l)
  (print (apnd (car l) (cdr l))))

(print "------------------------------")
(print "Darryl Stevenson - 1st Attempt")
(print "------------------------------")
(darryl)

