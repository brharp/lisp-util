
(defun match-char (char)
  (let ((inch (read-char)))
    (if (char= char inch) char
      (error "char mismatch: expected ~s but got ~s"
	     char inch))))

(defun word ()
  (do ((char (read-char) (read-char))
       (buffer (make-array 256 :fill-pointer 0)))
      ((not (alphanumericp char))
       (progn (unread-char char)
	      (coerce buffer 'string)))
      (vector-push char buffer)))

#+debug
(with-input-from-string
 (*standard-input* "Jan ")
 (word))
 
(defun cron-tab ()
  "Parses a cron file."
  (when (peek-char t nil nil)
    (cons (cron-line)
	  (cron-tab))))

(defun cron-line ()
  (list (minutes)
	(hours)
	(days)
	(months)
	(weekdays)
	(action)))

(defun minutes ()
  (field 0 60))

(defun hours ()
  (field 0 24))

(defun days ()
  (field 0 32))

(defun months ()
  (field 0 12))

(defun weekdays ()
  (field 0 7))

(defun action ()
  (read-line))

(defun num ()
  (let ((w (word)))
    (parse-integer w)))

(defun range-end ()
  (match-char #\-)
  (num))

(defun range (min max)
  (let ((lo (num)) hi)
    (case (peek-char t)
	  ((#\-) (setq hi (range-end)))
	  (t     (setq hi lo)))
    (cons lo hi)))


(defun field-list (min max)
  (match-char #\,)
  (field min max))

(defun field (min max)
  (list* (range min max)
	 (case (peek-char t)
	       ((#\,) (field-list min max))
	       (t     ()))))

#+debug
(with-input-from-string
 (*standard-input* "30 6-8 3,4-5,9 3 2 Comment")
 (cron-line))

