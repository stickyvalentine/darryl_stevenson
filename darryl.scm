(define (dothis l)
  (cond ((< (lslen? l) 2)
	 (print l)))
  (cond ((equal? (car l) "+")
	 (print(addall (cdr l)))))
  (cond ((equal? (car l) "-")
	 (print(suball (cdr l)))))
  (cond ((equal? (car l) '*)
	 (print(multall (cdr l)))))
  (cond ((equal? (car l) '/)
	 (print(divall (cdr l)))))
  (cond ((equal? (car l) 'double)
	 (print(double (cdr l)))))
  )

(define (darryl)
  (dothis (apnd (numberize_list (string-split (linenoise "Darryl> "))) 0)))

(define (numberize c)
  (cond ((integer? c) c)
	((string->number c) (string->number c))
	(else c)))

(define (numberize_list l)
  (cond ((not (null? l))
	 (cons (numberize (car l)) (numberize_list (cdr l))))
	(else l)))

(define (tokenize l)
  (let loop ((t '())
             (l l))
    (if (pair? l)
        (let ((c (car l)))
          (if (char=? c #\space)
              (cons (reverse t) (loop '() (cdr l)))
              (loop (cons (car l) t) (cdr l))))
        (if (null? t)
            '()
            (list (reverse t))))))

(define (string-split s)
  (map list->string (tokenize (string->list s))))

(define (delete-n list n)
  (if (= n 0)
      (cdr list)
      (cons (car list) (delete-n (cdr list) (- n 1)))))

(define (insert-n list item n)
  (if (= n 0)
      (cons item list)
      (cons (car list) (insert-n (cdr list) item (- n 1)))))

(define (list-nth list n)
  (if (= n 0)
      (car list)
      (list-nth (cdr list) (- n 1))))

(define (replace-nth list n item)
  (if (= n 0)
      (cons item (cdr list))
      (cons (car list) (replace-nth (cdr list) (- n 1) item))))

(define (swap-list-item list m n)
  (let
    ((a (list-nth list m))
     (b (list-nth list n)))
    (replace-nth
      (replace-nth list m b) n a)))

(define (lslen? l)
  (cond ((null? l) 0)
	((atom? l) 1)
	(else (+ 1 (lslen? (cdr l))))))

(define (apnd l i)
  (if (null? l)
      (list i)
      (cons (car l) (apnd (cdr l) i))))

(define (tail l)
  (list-nth l (+ -1 (lslen? l)) ) )

(define (>1 l)
  (if(> (lslen? l) 1)
     #t
     #f))
	
(define (addall l)
  (if (and (>1 l) (integer? (car l)))
      (addall (addone l))
      l))

(define (addone l)	  
  (replace-nth (cdr l)
	       (+ -1 (lslen? (cdr l)))
	       (+ (car l) (tail l)))
  )
