(defun range (start end)
  (if (eq start end)
      (list end)
      (cons start (range (1+ start) end))))

(find-if #'evenp (range 1 10))

(member 4 (range 1 10))


