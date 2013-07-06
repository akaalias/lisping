(defvar *cereals* '("Captain Crunch" "Cinnamon Toast Crunch" "Lucky Charms" "Frosted Flakes" "Cheerios" "Reeses Puffs" "Coco Pops"))

(defun atom? (x)
  (not (listp x)))

(defun my-first (x)
  (car x))

(defun lat? (l)
  (cond ((null l) t)
	((atom? (car l)) (lat? (cdr l)))
	(t nil)))

(lat? '((one) (two)))
(lat? '(a b c))

(defun member? (a lat)
  (cond ((null lat) nil)
	(t (or (eq (car lat) a)
	       (member? a (cdr lat))))))

(member? 'tea '(a b c tea))

(defun rember (a lat)
  (cond ((null lat) '())
	((eq (car lat) a) (cdr lat))
	(t (cons (car lat) 
		 (rember a (cdr lat))))))

(rember 'mint '(lamb something  mint abc chop final))

(defun firsts (l)
  (cond ((null l) '())
	(t (cons (car (car l)) 
		 (firsts (cdr l))))))

(defun keys (assoc-list)
  (firsts assoc-list))

(keys '((apple)
	  (peach some)
	  (oog)
	  (x)
	  ()
	  (bla)))

(defun rests (l)
  (cond ((null l) '())
	(t (cons (cdr (car l)) (rests (cdr l))))))

(rests '((apple yum)
	 (pear yum)
	 (onion ugh)))


(defun insertR (new old lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) old) (cons old (cons new (cdr lat))))
		 (t (cons (car lat) (insertR new old (cdr lat))))))))

(insertR 'a 'b '(o o b o))

(defun insertL (new old lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) old) (cons new (cons old (cdr lat))))
		 (t (cons (car lat) (insertR new old (cdr lat))))))))


(insertL 'a 'b '(o o b o))

(defun subst2 (new old lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) old) (cons new (cdr lat)))
		 (t (cons (car lat) (subst2 new old (cdr lat))))))))

(subst2 'd 'c '(a b c))

(defun subst3 (new old1 old2 lat)
  (cond ((null lat) '())
	(t (cond ((or (eq (car lat) old1)
		      (eq (car lat) old2)) (cons new (cdr lat)))
		 (t (cons (car lat) (subst3 new old1 old2 (cdr lat))))))))

(subst3 'd 'c 'e '(a b c e))

(defun multirember (a lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) a) (multirember a (cdr lat)))
		 (t (cons (car lat) (multirember a (cdr lat))))))))

(multirember 'a '(a b c e a))

(defun multiinsertR (new old lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) old) 
		  (cons old (cons new 
				  (multiinsertR new old (cdr lat)))))
		 (t (cons (car lat)
			  (multiinsertR new old (cdr lat))))))))

(multiinsertr 'fried 'fish '(chips and fish))

(defun multiinsertL (new old lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) old)
		  (cons new (cons old 
				  (multiinsertL new old (cdr lat)))))
		 (t (cons (car lat)
			  (multiinsertL new old (cdr lat))))))))

(multiinsertL 'fried 'fish '(chips and fish and more fish))

(defun multisubst (new old lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) old) (cons new (multisubst new old (cdr lat))))
		 (t (cons (car lat)
			  (multisubst new old (cdr lat))))))))

;; number games!

(atom? 13) ;;=> T
(atom? -1) ;;=> T

(1+ 100)

;; interlude: Can I write my own map function???
(defun my-map (f ls)
  (cond ((null ls) '())
	(t (cons (funcall f (car ls)) (my-map f (cdr ls))))))

(defun 10+ (x)
  (+ 10 x))

(my-map #'10+ '(1 2 3 4 5 6 7 8))
			       
;; 
(1+ 1)
(1- 1)

(defun o+ (n m)
  (cond ((zerop m) n)
	(t (1+ (o+ n (1- m))))))

(o+ 3 200)

(defun o- (n m)
  (cond ((zerop m) n)
	(t (1- (o- n (1- m))))))

(o- 100 100)

(defun addtup (tup)
  (cond ((null tup) 0)
	(t (+ (car tup) (addtup (cdr tup))))))

(addtup '(1 2 3 4 5))

(defun range (start end)
  (cond ((eq start end) '())
	(t (cons start (range (1+ start) end)))))

(range 1 10)

(addtup (range 1 100))

(defun omult (n m)
  (cond ((zerop m) 0)
	(t (+ n (omult n (1- m))))))


(omult 5 10)

(defun tup+ (tup1 tup2)
  (cond ((null tup1) tup2)
	((null tup2) tup1)
	(t (cons (+ 
		  (car tup1) (car tup2)) 
		 (tup+ (cdr tup1) (cdr tup2))))))

(tup+ '(1 2 3) '(1 2 3))

(defun o> (n m)
  (cond ((zerop n) nil)
	((zerop m) t)
	(t (o> (1- n) (1- m)))))

(o> 100 10)

(defun o< (n m)
  (cond ((zerop m) nil)
	((zerop n) t)
	(t (o< (1- n) (1- m)))))

(o< 100 100)

(defun o= (n m)
  (cond ((zerop m) (zerop n))
	((zerop n) nil)
	(t (o= (1- n) (1- m)))))

(defun o= (n m)
  (cond ((o< n m) nil)
	((o> n m) nil)
	(t t)))

(o= 1 1)

(defun oexpt (n m)
  (cond ((zerop m) 1)
	(t (* n (oexpt n (1- m))))))

(oexpt 5 5)

(defun o% (n m)
  (cond ((o< n m) 0)
	(t (1+ (o% (- n m) m)))))

(o% 10 5)

(defun olength (lat)
  (cond ((null lat) 0)
	(t (1+ (olength (cdr lat))))))

(olength (range 1 10))

(olength '())

(defun pick (n lat)
  (cond ((zerop (1- n)) (car lat))
	(t (pick (1- n) (cdr lat)))))

(pick 5 '(one two three four five))

(defun rempick (n lat)
  (cond ((zerop (1- n)) (cdr lat))
	(t (cons (car lat) 
		 (rempick (1- n) (cdr lat))))))

(rempick 4 (range 1 10))
    


