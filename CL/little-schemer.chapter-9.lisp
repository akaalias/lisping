(defpackage :little-schemer.chapter-9
  (:use :common-lisp 
	:little-schemer.core))

(defun looking (a lat)
  (keep-looking a (nth 0 lat) lat))

(defun keep-looking (a sorn lat)
  (cond ((numberp sorn) (keep-looking a (nth (1- sorn) lat) lat))
	(t (eq sorn a))))

(looking 'caviar '(6 2 4 caviar 5 7 3))

(defun shift (pair) 
  (build (first (first pair))
	 (build (second (first pair))
		(second pair))))

;; (shift '((a b) c)) => (a (db c))
;; (shift '((a b) (c d))) => (a (b (c d)))

(defun align (pora)
  (cond ((atom?  pora) pora)
	((a-pair? (first pora))
	 (align (shift pora)))
	(t (build (first pora)
		  (align (second pora))))))

(defun length* (pora)
  (cond ((atom? pora) 1)
	(t (+ (length* (first pora))
	      (length* (second pora))))))

(length* '(a (b c)))

(defun weight* (pora)
  (cond ((atom? pora) 1)
	((null pora) 0)
	(t (+ (* (weight* (first pora)) 2)
	      (weight* (second pora))))))

(weight* '(a (b c)))

(defun shuffle (pora)
  (cond ((atom? pora) pora)
	((a-pair? (first pora))
	 (shuffle (revpair pora)))
	(t (build (first pora)
		  (shuffle (second pora))))))

(defun collatz (n)
  (cond ((= 1 n) 1)
	(t (cond ((evenp n) (collatz (/ n 2)))
		 (t (collatz (1+ (* 3 n))))))))

(defun ackermann (n m)
  (cond ((zerop n) (1+ m))
	((zerop m) (ackermann (1- n) 1))
	(t (ackermann (1- n)
		      (ackermann n (1- m))))))

(defun will-stop? (f) 
  (funcall f '()))


(length* '((a b) (c d)))

;; mk-length stuff. not clear about what it does.
