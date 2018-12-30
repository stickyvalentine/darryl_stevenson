(load "list-man.scm")
(load "math.scm")

(define (doo l)
  (cond ((< (lslen? l) 2) l)
	((equal? (car l) "+") (doo (add+ (cdr l))))
	((equal? (car l) "-") (doo (sub+ (cdr l))))
	((equal? (car l) "say") (doo (say (cdr l))))
	((equal? (car l) "append") (doo (append (cdr l))))
	((equal? (car l) "head") (doo (head (cdr l))))
	((equal? (car l) "tail") (doo (tail (cdr l))))
	((equal? (car l) "insert") (doo (insert (cdr l))))
	(else l)))
			
(define (darryl)
  (print (doo (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))
  (darryl))

  
(define (say l)
  (if (print (car l))
      (cdr l)
      (cdr l)))

(define (head l)
  (if (< (lslen? l) 3)
      (list (car l))
      (cons (car l) (head (cdr l)))))

(define (tail l)
  (if (< (lslen? l) 2)
      (car l)
      (tail (cdr l))))

(define (insert l)
  (if (integer? (cadr l))
      (insert-n (cdr l) (car l) (cadr l))
      l))

(print "------------------------------")
(print "Darryl Stevenson - 1st Attempt")
(print "------------------------------")
(darryl)

