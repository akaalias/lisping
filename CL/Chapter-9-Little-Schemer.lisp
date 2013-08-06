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

(shift '((a b) (c d)))
