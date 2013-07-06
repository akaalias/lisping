(defparameter *names* '("Alexis" "Jason" "Mattan" "Chris"))
(defparameter *locations* '("New York" "Sunnyvale" "San Francisco"))

(defstruct person age name location)

(defun random-age ()
  (random 80))

(defun pick-random (lst)
  (nth (random (length lst)) lst))

(defun random-name ()
  (pick-random *names*))

(defun random-location ()
    (pick-random *locations*))

(random-location)

(let ((alexis (make-person :age 23 :name "Alexis" :location "Silicon Valley")))
  (person-name alexis)
  (person-age alexis)
  (person-location alexis)
  )
