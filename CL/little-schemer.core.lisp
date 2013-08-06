(defpackage :little-schemer.core
  (:use :common-lisp))

(defun atom? (x) 
  (not (listp x)))

(defun pick (n lat)
  (cond ((zerop (1- n)) (car lat))
	(t (pick (1- n) (cdr lat)))))

(defun build (s1 s2)
  (cons s1 (cons s2 nil)))

(defun a-pair? (x)
  (cond ((null x) nil)
	((atom? x) nil)
	((null (cdr x)) nil)
	((null (cdr (cdr x))) t)
	(t nil)))

(defun revpair (p)
  (cons (second p) 
	(cons (first p) nil)))


