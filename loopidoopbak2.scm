(load "list-man.scm")

(define (dothis l)
  (if (and (> (lslen? l) 1) (in_list? (car l) '("+" "-")))
      (cond ((equal? (car l) "+") (addall (cdr l)))
	    ((equal? (car l) "-") (sub+ (cdr l)))
	    )
      l))

(define (darryl)
  (print (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0))))

;;; For testing
(define (do_int_eater l)
  (if (and (> (lslen? l) 2) (integer? (car l)))
      (do_int_eater (cdr l))
      (dothis l)))

(define (in_list? i l)
  (if (null? l)
      #f
      (or (equal? (car l) i) (in_list? i (cdr l)))))

(define (addall l)
  (if (and (>1 l) (integer? (car l)))
      (addall (addone l))
      (dothis l)))

(define (addone l)
  (replace-nth (cdr l)
               (+ -1 (lslen? (cdr l)))
               (+ (car l) (tail l)))
  )

(define (suball l)
  (if (and (>1 l) (integer? (car l)))
      (suball (subone l))
      (dothis l)))

(define (subone l)
  (replace-nth (cdr l)
               (+ -1 (lslen? (cdr l)))
               (- (tail l) (car l)))
  )

(define (sub+ l)
  (if (and (>1 l) (integer? (car l)))
      (sub+ (replace-nth (cdr l)
			 (+ -1 (lslen? (cdr l)))
			 (- (tail l) (car l))))
      (dothis l)))

(darryl)
