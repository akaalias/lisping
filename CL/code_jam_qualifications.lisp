(defvar *cyphers* '(e j p m y s l j y l c k d k x v e d d k n m c r e j s i c p d r y s i r b c p c y p c r t c s r a d k h w y f r e p k y m v e d d k n k m k r k c d d e k r k d e o y a k w a e j t y s r r e u j d r l k g c j v u o))

(defvar *keys* '(o u r l a n g u a g e i s i m p o s s i b l e t o u n d e r s t a n d t h e r e a r e t w e n t y s i x f a c t o r i a l p o s s i b i l i t i e s s o i t i s o k a y i f y o u w a n t t o j u s t g i v e u p j k))

(defun my-map (fn l)
  (cond ((null l) '())
	(t (cons (funcall fn (car l)) (my-map fn (cdr l))))))

(defun make-mappings (cyphers keys)
  (cond ((null cyphers) '())
	(t (cons 
	    (cons (car cyphers)
		  (cons (car keys) nil))
	    (make-mappings (cdr cyphers) (cdr keys))))))

(defvar *mappings* (remove-duplicates 
		    (sort 
		     (make-mappings *cyphers* *keys*) #'(lambda (x y)
					   (> (char-code (character x))
					      (char-code (character y))))
		     :key #'car) 
		    :test #'equal))
      
*mappings*

(defun get-key-for-cypher (cypher mappings)
  (car (cdr (assoc cypher mappings))))

(defun translate-map (cypher-text)
    (mapcar #'(lambda (x) (get-key-for-cypher x *mappings*)) cypher-text))

(translate-map '(e j p m y s l j y l c k d k x v e d d k n m c r e j s i c p d r y s i))

(defun translate (cypher)
  (cond ((null cypher) '())
	((cond ((null (get-key-for-cypher (car cypher) *mappings*)) (cons (cons (car cypher) 'unmapped) (translate (cdr cypher))))
	       (t (cons (get-key-for-cypher (car cypher) *mappings*) (translate (cdr cypher))))))))

(translate '(e j p m y s l j y l c k d k x v e d d k n m c r e j s i c p d r y s i))
(translate '(r b c p c y p c r t c s r a d k h w y f r e p k  y m v e d d k n k m k r k c d))
(translate '(d e k r k d e o y a k w a e j t y s r r e u j d r l k g c j v))

(translate '(y n f i c w l b k u o m x s e v z p d r j g a t h a q s e t k o s e t x a y n f d))
(translate '(s c h r r k x c t e s r a e j d k s l t k r b x c))
(translate '(w e p r b e d c t b e d v c y o k s y r e s l j c i e s e r d v c y o r e e r b c p v c e v m c)) 
(translate '(s e n e i a j s i c p d r y s i d r b c x d k s f c r b c a y p c d v c y o k s l x a d r c p k c d k s r b c d v k p k r))

(defun string-to-char-list (s)
  (coerce s 'list))

(defun char-list-to-string (cl)
  (coerce cl 'string))

(let ((s "seneia jsicpdrysid rbcx dksfc rbca ypc dvcyoksl xadrcpkcd ks rbc dvkpkr"))
  (equal s (char-list-to-string (string-to-char-list s))))


