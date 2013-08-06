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

(defun seconds (l)
  (cond ((null l) '())
	(t (cons (car (cdr (car l)))
		 (seconds (cdr l))))))

(seconds '((a b) (c d)))


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
	(t (cond ((equal? (car lat) a) (multirember a (cdr lat)))
		 (t (cons (car lat) (multirember a (cdr lat))))))))

(multirember 'a '(a b c e a))

(defun multiinsertR (new old lat)
  (cond ((null lat) '())
	(t (cond ((eq (car lat) old) 
		  (cons old (cons new 
				  (multiinsertR new old (cdr lat)))))
		 (t (cons (car lat)
			  (multiinsertR new old (cdr lat))))))))

(multiinsertr 'fried 'fish '(fish chips and fish))

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
	(t (cons (funcall f (car ls)) 
		 (my-map f (cdr ls))))))

(defun 10+ (x)
  (+ 10 x))

(my-map #'10+ '(1 2 3 4 5 6 7 8))
;; => (11 12 13 14 15 16 17 18)
			       
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
    
(defun no-nums (lat)
  (cond ((null lat) '())
	(t (cond ((numberp (car lat)) (no-nums (cdr lat)))
		 (t (cons (car lat) 
			  (no-nums (cdr lat))))))))

(no-nums '(1 2 3 abc 1 2 3 def))
(no-nums '(1 2 3))

(defun all-nums (lat)
  (cond ((null lat) '())
	(t (cond ((numberp (car lat)) (cons (car lat)
					    (all-nums (cdr lat))))
		 (t (all-nums (cdr lat)))))))

(all-nums '(1 2 3 a))

(defun equan? (a1 a2)
  (cond 
    ((and (numberp a1) 
	  (numberp a2))
     (= a1 a2))
    ((or (numberp a1)
	 (numberp a2))
     nil)
    (t (eq a1 a2))))

(defun occur (a lat)
  (cond 
    ((null lat) 0)
    (t (cond ((equan? (car lat) a) (1+ (occur a (cdr lat))))
	     (t (occur a (cdr lat)))))))

(occur 'a '(b c ))

(defun one? (n)
   (= n 1))

(one? 0)
(one? 1)

(defun rempick (n lat)
  (cond 
    ((one? n) (cdr lat))
    (t (cons (car lat) (rempick (1- n) (cdr lat))))))

(rempick 3 (range 1 10))

;; Chapter 5

(defun rember* (a l)
  (cond 
    ((null l) '())
    ((atom (car l))
     (cond 
       ((eq (car l) a) 
	(rember* a (cdr l)))
       (t (cons (car l)
		(rember* a (cdr l))))))
    (t (cons (rember* a (car l))
	     (rember* a (cdr l))))))

(rember* 'a '((a b) (c d) a (b a)))
(lat? '(a b c))
(lat? '((a b) (c d) a (b a)))


(defun insertR* (new old l)
  (cond ((null l) '())
	((atom (car l)) 
	 (cond ((eq (car l) old) (cons old 
				       (cons new 
					     (insertR* new old (cdr l)))))
	       (t (cons (car l) 
			(insertR* new old (cdr l))))))
	(t (cons (insertR* new old (car l))
		 (insertR* new old (cdr l))))))

(insertR* 'a 'b '(((b (b) c)) b))

(defun occur* (a l)
  (cond ((null l) 0)
	((atom (car l))
	 (cond ((eq (car l) a)
		(1+ (occur* a (cdr l))))
	       (t (occur* a (cdr l)))))
	(t (+ (occur* a (car l))
	      (occur* a (cdr l))))))

(occur* 'a '((a b c) d (e f g a)))

(defun subst* (new old l)
  (cond ((null l) '())
	((atom (car l))
	 (cond ((eq (car l) old) (cons new
				       (subst* new old (cdr l))))
	       (t (cons (car l)
			(subst* new old (cdr l))))))
	(t (cons (subst* new old (car l))
		 (subst* new old (cdr l))))))

(subst* 'a 'b '(((((((((((((((((((b c))))))))))))))))))))

(defun insertL* (new old l)
  (cond ((null l) '())
	((atom (car l)) 
	 (cond ((eq (car l) old) (cons new 
				       (cons old
					     (insertL* new old (cdr l)))))
	       (t (cons (car l) 
			(insertL* new old (cdr l))))))
	(t (cons (insertL* new old (car l))
		 (insertL* new old (cdr l))))))

(insertL* 'a 'b '(((b c))))

(defun member* (a l)
  (cond ((null l) nil)
	((atom? (car l))
	 (or (eq (car l) a)
	     (member* a (cdr l))))
	(t (or (member* a (car l))
	       (member* a (cdr l))))))

(member* 'a '((((b))(a))))

(defun leftmost (l)
  (cond ((atom? (car l)) (car l))
	(t (leftmost (car l)))))

(leftmost '(((a)(((((b)c)))))))

(defun eqlist? (l1 l2)
  (cond ((and (null l1) (null l2)) t)
	((or (null l1) (null l2)) nil)
	((and (atom? (car l1))
	      (atom? (car l2)))
	 (and (equan? (car l1) (car l2))
	      (eqlist? (cdr l1) (cdr l2))))
	((or (atom? (car l1))
	     (atom? (car l2)))
	 nil)
	(t 
	 (and (eqlist? (car l1) (car l2))
	      (eqlist? (cdr l1) (cdr l2))))))

(eqlist? '(a b c) '(a b c))

(defun equal? (s1 s2)
  (cond ((and (atom? s1) (atom? s2))
	 (equan? s1 s2))
	((or (atom? s1) nil)
	 (atom? s2) nil)
	(t (eqlist? s1 s2))))

(equal? '(a b) '(a b c))

(defun eqlist? (l1 l2)
  (cond ((and (null l1) (null l2)) t)
	((or (null l1) (null l2)) nil)
	(t
	 (and (equal? (car l1) (car l2))
	      (eqlist? (cdr l1) (cdr l2))))))

(eqlist? '(a) '(a))

(defun rember (s l)
  (cond ((null l) nil)
	((equal? (car l) s) (cdr l))
	(t 
	 (cons (car l)
	       (rember s (cdr l))))))

(rember 'a '(a b c a))

;; Chapter 6

(defun ^ (a b)
  (expt a b))

(^ 3 3)

(defun numbered? (aexp)
  (cond ((atom? aexp) (numberp aexp))
	((eq (car (cdr aexp)) '+)
	 (and (numbered? (car aexp))
	      (numbered? (car (cdr (cdr aexp))))))
	((eq (car (cdr aexp)) '*)
	 (and (numbered? (car aexp))
	      (numbered? (car (cdr (cdr aexp))))))
	((eq (car (cdr aexp)) '^)
	 (and (numbered? (car aexp))
	      (numbered? (car (cdr (cdr aexp))))))
	(t (and (numbered? (car aexp))
		(numbered? (car (cdr (cdr aexp))))))))

(numbered? '(1 + 2 (1 + 2 (2))))

(defun value (nexp)
  (cond ((atom nexp) nexp)
	((eq (car (cdr nexp)) '+)
	 (+ (value (car nexp))
	    (value (car (cdr (cdr nexp))))))
	((eq (car (cdr nexp)) '*)
	 (* (value (car nexp))
	    (value (car (cdr (cdr nexp))))))
	(t
	 (^ (value (car nexp))
	    (value (car (cdr (cdr nexp))))))))

(value '(1 + (1 + 3)))

(defun 1st-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car aexp))

(defun value (nexp)
  (cond ((atom nexp) nexp)
	((eq (operator nexp) '+)
	 (+ (value (1st-sub-exp nexp))
	    (value (2nd-sub-exp nexp))))
	((eq (operator nexp) '*)
	 (* (value (1st-sub-exp nexp))
	    (value (2nd-sub-exp nexp))))
	(t 
	 (^ (value (1st-sub-exp nexp))
	    (value (2nd-sub-exp nexp))))))

(value '(+ 1 (+ 2 3)))

(defun sero? (n)
    (null n))

(defun edd1 (n)
  (cons '() n))

(edd1 (edd1 (edd1 '())))

(defun zub1 (n)
  (cdr n))

(zub1 (edd1 (edd1 (edd1 '()))))

(defun plus (n m)
  (cond ((sero? m) n)
	(t (edd1 (plus n (zub1 m))))))

(plus '(()) '(() () ()))
    
(defun member? (a lat)
  (cond ((null lat) nil)
	(t (or (equal? (car lat) a)
	       (member? a (cdr lat))))))

(member? 1 '())

(defun set? (lat)
    (cond ((null lat) t)
	  ((member? (car lat) (cdr lat)) nil)
	  (t (set? (cdr lat)))))

(set?  '(a b c 1 1))
(set? (range 1 100))

(defun makeset (lat)
  (cond ((null lat) '())
	((member? (car lat) (cdr lat)) (makeset (cdr lat)))
	(t (cons (car lat) (makeset (cdr lat))))))

(makeset (cons 1 (range 1 10)))
	 
(defun makeset (lat)
  (cond ((null lat) '())
	(t 
	 (cons (car lat)
	       (makeset 
		(multirember (car lat) (cdr lat)))))))

(makeset '(1 2 3 4 5 6 7 1 7 8 9))

(defun subset? (set1 set2)
  (cond ((null set1) t)
	((member? (car set1) set2) 
	 (subset? (cdr set1) set2))
	(t 
	 nil)))

(subset? '(a b) '(c d a b))

(defun eqset? (set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(eqset? '(A B C) '(B C A))

(defun count-atoms* (l)
  (cond ((null l) 0)
	((atom? (car l))
	 (1+ (count-atoms* (cdr l))))
	(t 
	 (+ (count-atoms* (car l))
	    (count-atoms* (cdr l))))))

(count-atoms* (range 1 10))

(defun intersect? (set1 set2)
  (cond ((null set1) nil)
        (t
	 (or (member? (car set1) set2)
	     (intersect? (cdr set1) set2)))))

(intersect? '(a b) '(a b c d e))

(defun my-map (fn l)
  (cond ((null l) '())
	(t (cons (funcall fn (car l))
		 (my-map fn (cdr l))))))

(my-map #'1+ '(1 2 3 4)) ;; => (2 3 4 5)


(defun intersect (set1 set2)
  (cond ((null set1) '())
	((member? (car set1) set2) (cons (car set1) 
					 (intersect (cdr set1) set2)))
	(t (intersect (cdr set1) (cdr set2)))))

(intersect '(1 2 3) '(2 3 4 5 1))

(defun union+ (set1 set2)
  (cond ((null set1) '())
	((member? (car set1) set2) (union (cdr set1) set2))
	(t (cons (car set1) (union (cdr set1) set2)))))

(union+ '(1 2 3) '(2 3 4 5))

(defun intersectall (l-set)
  (cond ((null (cdr l-set)) (car l-set))
	(t (intersect (car l-set)
		      (intersectall (cdr l-set))))))

(intersectall '((1 2)
		(2 1)
		(1 2)
		(1 2 4 5 6)))
	 
(defun a-pair? (x)
  (cond ((null x) nil)
	((atom? x) nil)
	((null (cdr x)) nil)
	((null (cdr (cdr x))) t)
	(t nil)))

(a-pair? '(a (b) (c)))

(defun first~ (x)
  (car x))

(defun second~ (x)
  (car (cdr x)))

(defun third~ (x)
  (car (cdr (cdr x))))

(defun build (s1 s2)
  (cons s1 (cons s2 '())))

(first~ (build 'a '(b)))
(second~ (build 'a '(b)))
(third~ (build 'a '(b)))

(defun fun? (rel)
  (set? (firsts rel)))

(fun? '((a b)
	(c d)
	(e f)))

(defun revrel (rel)
  (cond ((null rel) '())
	(t (cons (build (second (car rel))
			(first (car rel)))
		 (revrel (cdr rel))))))

(revrel '((1 2) (3 4) (5 6)))

(defun revpair (pair)
  (build (second pair)
	 (first pair)))

(defun revrel (rel)
  (cond ((null rel) '())
	(t (cons (revpair (car rel))
		 (revrel (cdr rel))))))


				
(defun fullfun? (fun)
  (set? (seconds fun)))

(one-to-one? '((a b) (c f)))

(defun one-to-one? (fun)
  (fun? (revrel fun)))


(equal '(1) '(1))

;; Chapter 8 LTU

(defun rember-f (test? a l)
  (cond ((null l) nil)
	((funcall test? (car l) a) (rember-f test? a (cdr l)))
	(t (cons (car l) 
		 (rember-f test? a (cdr l))))))

(rember-f #'equal 'a '(a (b) c a))
(rember-f #'= '1 '(1 2 3 4 5 1))

(defun eq?-c (a)
  (lambda (x)
    (eq x a)))

(setq eq?-salad (eq?-c 'salad))

(funcall (eq?-c 'salad) 'salad)

(defun rember-f (test?)
  (lambda (a l)
    (cond ((null l) '())
	  ((funcall test? (car l) a) (funcall (rember-f test?) a (cdr l)))
	  (t (cons (car l) (funcall (rember-f test?) a (cdr l)))))))

(setq rember-eq (rember-f #'eq))
(setq rember-equal (rember-f #'equal))

(funcall rember-eq 1 '(1 1 1 2 3 4  1 2 123 12))
(funcall rember-equal '(a b) '((a b) (c d)))
(funcall (rember-f #'eq) '1 '(1 2 3 4)) 


(defun seqL (new old l)
  (cons new (cons old l)))

(defun seqR (new old l)
  (cons old (cons new l)))


(defun insert-g (seq)
  (lambda (new old l)
    (cond ((null l) '())
	  ((eq (car l) old) (funcall seq new old (cdr l)))
	  (t 
	   (cons (car l) 
		 (funcall (insert-g seq) new old (cdr l)))))))

(funcall (insert-g #'seqL) 'a 'b '(b c d e))
(funcall (insert-g #'seqR) 'b 'a '(a c d e))

(setq insertL (insert-g (lambda (new old l)
			  (cons new (cons old l)))))

(funcall insertL 'a 'b '(b c d e))

(defun seqS (new old l)
  (cons new l))

(setq subst (insert-g #'seqS))

(funcall subst 'a 'b '(b c d e))
	   
(defun seqrem (new old l)
  l)

(setq rem (insert-g #'seqrem))

(funcall rem 'a 'b '(a b c d e))

(defun atom-to-function (x)
  (cond ((eq x '+) #'+)
	((eq x '*) #'*)
	(t #'^)))

(funcall (atom-to-function '+) 3 4)

(defun value (nexp)
  (cond ((atom? nexp) nexp)
	(t (funcall (atom-to-function (operator nexp))
		    (value (1st-sub-exp nexp))
		    (value (2nd-sub-exp nexp))))))

(value '(+ 1 10))
				     
;; skipping the multirembert and col

;; Chapter 9

(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))

(defun keep-looking (a sorn lat)
  (cond ((numberp sorn) (keep-looking a (pick sorn lat) lat))
	(t (eq sorn a))))

(looking 'caviar '(6 2 4 caviar 5 7 3))

(defun shift (pair)
  (build (first (first pair))
	 (build (second (first pair))
		(second pair))))

(shift '((a b) (c d)))

(defun weight* (pora)
  (cond ((atom? pora) 1)
	(t (+ (* (weight* (first pora)) 2)
	      (weight* (second pora))))))

(weight* '(a (b c)))

(defun shuffle (pora)
  (cond ((atom? pora) pora)
	((a-pair? (first pora)) (shuffle (revpair pora)))
	(t (build (first pora)
		  (shuffle (second pora))))))

(shuffle '(a (b c)))

(defun C (n)
  (cond 
    ((= 1 n) 1)
    (t 
     (cond ((evenp n) (C (/ n 2)))
	   (t (C (1+ (* 3 n))))))))

(C 20)
	 



