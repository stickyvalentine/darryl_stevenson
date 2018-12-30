(load "list-man.scm")
(load "math.scm")


(define (dothis l)
  (cond ((> (lslen? l) 1)
	 (cond ((list? l)
		(cond ((in_list? (car l) '("+" "-" "say" "append" "quit" "head" "tail" "insert"))
		       (dothisalltheway l))
		      (else l)))
	       (else l)))
	(else l))
  )

(define (anotherdo)
  (if

(define (dothisthing l)
  (cond ((equal? (car l) "+") (add+ (cdr l)))
	((equal? (car l) "-") (do_sub+ (cdr l)))
	((equal? (car l) "say") (say (cdr l)))
	((equal? (car l) "append") (append (cdr l)))
	((equal? (car l) "head") (head (cdr l)))
	((equal? (car l) "tail") (tail (cdr l)))
	((equal? (car l) "insert") (tail (cdr l)))	
	(else (cons "$" l))))

(define (dothisalltheway2 l)
  (if (or (< (lslen? l)  2) (equal? (car l) "$"))
      l
      (dothisalltheway (dothisthing l))))

(define (dothisalltheway l)
  (cond ((< (lslen? l) 2) l)
	((equal? (car l) "$") (cdr l))
	(else (dothisalltheway (dothisthing l)))))

(define (darryl)
  (print (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))
  (darryl))

(define (darryl-no-loop)
  (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))
  
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

(define (add+ l)
  (if (and (>1 l) (integer? (car l)))
      (add+ (replace-nth (cdr l)
                         (+ -1 (lslen? (cdr l)))
                         (+ (car l) (tail l))))
      l))

(print "------------------------------")
(print "Darryl Stevenson - 1st Attempt")
(print "------------------------------")
;;;(darryl)

