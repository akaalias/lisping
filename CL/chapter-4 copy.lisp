(defparameter *foods* '(CHICKEN BEEF PORK VEGGIES FRUIT))

(if '()
    'true
    'false)

(if ()
    't
    'f)

(if '(1)
    't
    'f)

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

(my-length *foods*)

(and (eq '() nil)
     (eq '() ())
     (eq '() nil))

(if (= (+ 1 2) 3)
    't
    'f)

(if '(1)
    't
    'f)

(if '()
    't
    'f)

(oddp 5)

(if (oddp 6)
    'odd
    'even)

(if (oddp 5)
    'odd
    (/ 1 1))

(defvar *number-was-odd* nil)

(if (oddp 7)
    (progn (setf *number-was-odd* t)
	   'odd-number)
    (progn (setf *number-was-odd* nil)
    'even-number))

(defvar *number-is-odd* nil)

(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)

*number-is-odd*
;; pudding eater??
	 
	 
(member 1 '(1 2 3))
(member-if #'oddp '(2 2 3))

(defun my-length-2 (list)
  (if (eq nil list)
      0
      (+ 1 (my-length (cdr list)))))

(my-length-2 '(1 2 3 4 5 6))

(defun member? (x list)
  (if (eq (car list) x)
      list
      (member x (cdr list))))

(member? 4 '(1 2 3 4))

(defun range (start end)
  (if (= start end)
      (list end)
      (cons start (range (1+ start) end))))

(range 1 10)

(defun my-count (nested-list)
  (cond ((eq nil nested-list) 0)
	((atom nested-list) 1)
	(t (+ (my-count (car nested-list))
	      (my-count (cdr nested-list))))))
	   
(my-count '((1 2)(1 20 5)))

(mapcar #'1- (range 1 4))

(mapcar #'1+ (range 10 1000))

(character "A")
(string #\A)
(char-code #\A)

(char-key #\B)
(char-code #\A)

