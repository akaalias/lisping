(defun nil? (x)
  (eq x nil))

(defun my-assoc (key list)
  (cond ((nil? list) '())
	((eq key (caar list)) (car list))
	(t (my-assoc key (cdr list)))))

(my-assoc 'test '((test (one two three))))

(defun my-range (start end)
  (cond ((= start end) (list end))
	(t (cons start (my-range (1+ start) end)))))

(my-range 1 100)

(defun my-member? (elt list)
  (cond ((nil? list) '())
	((eq elt (car list)) list)
	(t (my-member? elt (cdr list)))))

(defvar *my-map* '((:one (:values :for :one))
		   (:two (:values :for :two))))

(equal (my-member? 8 (my-range 1 10))
       (member 8 (my-range 1 10)))

(equal (assoc :one *my-map*)
       (my-assoc :one *my-map*))

(defun my-count (list)
  (cond ((equal list nil) 0)
	(t (1+ (my-count (cdr list))))))

(my-count (my-range 1 100))
