
(defun load-notes (path))

(defun save-notes (path))

(defun new-note ()
  (let* ((gn (get-string "First"))
         (sn (get-string "Last"))
         (s (get-string "S"))
         (o (get-string "O"))
         (a (get-string "A"))
         (p (get-string "P")))
    (make-instance 
