(defun hello ()
  "Hello")

(defparameter *foods* '(chicken beef pork mouse))

(hello)

(cdr (cons 'a 'b)) 
(cdr (cons 'chicken 'nil))

(expt 2 3)

(cdr (cons 'beef (cons 'chicken 'test)))

(cdr *foods*)
(car (cdr *foods*))

(list "abc" "DEF")
(rest *foods*)

(car (car (list 'a 'b 'c '(d e f))))
