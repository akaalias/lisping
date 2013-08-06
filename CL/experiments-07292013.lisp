(defpackage :little-schemer.experiments-07292013
  (:use :common-lisp))

(defparameter *apartment-criteria* '(clean sunny quiet 1800 l-train 4-th-floor 1-year empty))
(defparameter *sublet-criteria* '(clean sunny quiet 1200 l-train 4-th-floor 3-months furnished nice-roomies))

(defun member? (a lat)
  (cond ((null lat) nil)
	((atom? (car lat))
	 (or (eq (car lat) a)
	     (member? a (cdr lat))))
	(t (or (member? a (car lat))
	       (member? a (cdr lat))))))

(member? 'a '(b c a))

(defun intersect (a b)
  (cond ((null a) '())
	((member? (car a) b)
	 (cons (car a) 
	       (intersect (cdr a) b)))
	(t (intersect (cdr a) b))))

(intersect *apartment-criteria* *sublet-criteria*)

