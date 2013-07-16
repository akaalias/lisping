(defvar *cypher* 
  "ejp mysljylc kd kxveddknmc re jsicpdrysi rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd de kr kd eoya kw aej tysr re ujdr lkgc jv z q 1 2 3 4 5 6 7 8 9 0")

(defvar *key* 
  "our language is impossible to understand there are twenty six factorial possibilities so it is okay if you want to just give up q z 1 2 3 4 5 6 7 8 9 0")

(defun interleave (l1 l2)
  (cond ((null l1) '())
	(t (cons (cons (car l1) (cons (car l2) nil)) (interleave (cdr l1) (cdr l2))))))

(defvar *pairs* (remove-duplicates 
		 (interleave (coerce *cypher* 'list) 
			     (coerce *key* 'list)) 
		 :key #'car))

(defun translate-character (c)
  (cond ((null (assoc c *pairs*)) #\?)
	(t (cadr (assoc c *pairs*)))))

(defun translate-string (s)
  (coerce (mapcar #'translate-character (coerce s 'list)) 'string))

(defun translate-file (in)
  (let ((db nil))
    (with-open-file (stream in)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(push (translate-string line) db)))
    db))

(defun write-out (data out-file)
  (with-open-file (stream out-file :direction :output)
    (mapcar #'(lambda (line) (WRITE-LINE line stream)) data)))

(write-out (reverse 
	    (translate-file "/Users/alexisrondeau/The Great Lisp Adventure/CL/A-small-practice.in"))
	   "/Users/alexisrondeau/The Great Lisp Adventure/CL/A-small-practice.out")
