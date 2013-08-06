(defpackage :little-schemer.chapter-10
  (:use :common-lisp 
	:little-schemer.core))

(defparameter *table* '(((appetizer entree beverage)
			   (pate boef vin))
			  ((appetizer entree beverage)
			   (beer beer beer))
			  ((appetizer entree beverage)
			   ((food is) (number one with us)))))

(defun new-entry (things)
  (list '(appetizer entree beverage)
	things))

(new-entry '(beer beer beer))
(new-entry '(pate boef vin))

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help 
   name
   (first entry)
   (second entry)
   entry-f))

(defun lookup-in-entry-help (name names values entry-f)
  (cond ((null names) (entry-f name))
	((eq (car names) name)
	 (car values))
	(t (lookup-in-entry-help 
	    name
	    (rest names)
	    (rest values)
	    entry-f))))

*table*
