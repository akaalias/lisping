(defvar *choices* '(Gym Lisp Copy))

(defun member? (a lat)
  (cond ((null lat) nil)
	(t (cond ((eq (car lat) a) t)
		 (t (member? a (cdr lat)))))))

(defun compare (a b)
  (progn
    (format t "Please choose: ~a or ~a: " a b)
    (let ((input (read)))
      (cond ((or (eq input a)
		 (eq input b)) input)
	    (t (compare a b))))))

(defun compare-list (a lat)
  (cond ((null (car lat)) '())
	(t 
	 (cons (compare a (car lat))  
	       (compare-list a (cdr lat))))))

(defun rank (lat)
  (cond ((null lat) '())
	(t (cons (compare-list (car lat) (cdr lat))
		 (rank (cdr lat))))))	

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun rank-assoc (choices ranks)
  (cond ((null choices) nil)
	(t 
	 (cons (list (car choices) (count (car choices) ranks)) 
	       (rank-assoc (cdr choices) ranks)))))

(defun rank-choices (choices)
  (rank-assoc choices (flatten (rank choices))))


