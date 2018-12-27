(define (do_int_eater l)
  (if (and (> (lslen? l) 2) (integer? (car l)))
      (do_int_eater (cdr l))
      (dothis l)))

(define (do_add+ l)
  (if (and (>1 l) (integer? (car l)))
      (do_add+ (replace-nth (cdr l)
                         (+ -1 (lslen? (cdr l)))
                         (+ (car l) (tail l))))
      (dothis l)))

(define (do_sub+ l)
  (if (and (>1 l) (integer? (car l)))
      (do_sub+ (replace-nth (cdr l)
                         (+ -1 (lslen? (cdr l)))
                         (- (tail l) (car l))))
      (dothis l)))

(define (do_mul+ l)
  (if (and (>1 l) (integer? (car l)))
      (do_mul+ (replace-nth (cdr l)
                         (+ -1 (lslen? (cdr l)))
                         (* (tail l) (car l))))
      (dothis l)))

(define (do_div+ l)
  (if (and (>1 l) (integer? (car l)))
      (do_div+ (replace-nth (cdr l)
                         (+ -1 (lslen? (cdr l)))
                         (/ (tail l) (car l))))
      (dothis l)))
