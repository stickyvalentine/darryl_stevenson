(load "list-man.scm")
(load "math.scm")

;;; List of ops. Used in isop?
(define ops '("+" "-" "say" "append" "head" "tail" "insert" "op?" "last_op" "car_inlist?"))

;;; Returns #t if item is an op.
(define (isop? i)
  (if (inlist? i ops)
      #t
      #f))

(define (doo l)
  (cond ((< (lslen? l) 2) l)
	((equal? (car l) "+") (add+ (cdr l)))
	((equal? (car l) "-") (sub+ (cdr l)))
	((equal? (car l) "say") (say (cdr l)))
	((equal? (car l) "append") (append (cdr l)))
	((equal? (car l) "head") (head (cdr l)))
	((equal? (car l) "tail") (tail (cdr l)))
	((equal? (car l) "insert") (insert (cdr l)))
	((equal? (car l) "op?") (isop? (cdr l)))
	((equal? (car l) "last_op") (last_op (cdr l)))
	((equal? (car l) "inlist?") (car_inlist? (cdr l)))
	(else l)))

;;; Prompt. Loops.
(define (darryl)
  (print (doo (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))
  (darryl))

;;; For testing. Only returns list from input.
(define (dar)
  (numberize_list (string-split (linenoise "Dar> "))))

;;; Returns #t if item is in list
(define (inlist? i l)
  (cond ((null? l) #f)
	((equal? i (car l)) #t)
	(else (inlist? i (cdr l)))))

;;; Returns #t if the next item in list is also in the rest of the list
(define (car_inlist? l)
  (inlist? (car l) (cdr l)))

;;; Returns the number of times an item occurs in the list
(define (n_inlist? i l)
  (number_inlist? l i 0))

;;; Helper for n_inlist?
(define (number_inlist? l i n)
  (if (> (lslen? l) 0)
      (if (equal? (car l) i)
	  (number_inlist? (cdr l) i (+ n 1))
	  (number_inlist? (cdr l) i n))
       n))
      
;;; Combines two lists, one after the other.
(define (combine l1 l2)
  (if (< (lslen? l2) 1)
      l1
      (combine (apnd l1 (car l2)) (cdr l2))))

;;; Returns from last op to the end of the list
(define (last_op l)
  (find_last_op (head l) (list (tail l)) ))

;;; Helper for last_op
(define (find_last_op la lb)
  (if (< (lslen? la) 2)
      #f
      (if (isop? (car lb))
	  lb
	  (find_last_op (head la) (cons (tail la) lb)))))
			
;;; Prints next list item
(define (say l)
  (if (print (car l))
      (cdr l)
      (cdr l)))

;;; Returns list without last item
(define (head l)
  (if (< (lslen? l) 3)
      (list (car l))
      (cons (car l) (head (cdr l)))))

;;; Returns only last item of list
(define (tail l)
  (if (< (lslen? l) 2)
      (car l)
      (tail (cdr l))))

;;; Not finished
(define (insert l)
  (if (integer? (cadr l))
      (insert-n (cddr l) (car l) (cadr l))
      l))

(print "------------------------------")
(print "Darryl Stevenson - 1st Attempt")
(print "------------------------------")
;;;(darryl)

