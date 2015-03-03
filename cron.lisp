
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
  (field 0 12 '("Xyz" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		"Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defun weekdays ()
  (field 0 7 '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))

(defun action ()
  (peek-char t)
  (read-line))

(defun num ()
  (parse-integer (word)))

(defun name (accept)
  (position (word) accept :test #'equal))

(defun term (accept)
  (if (digit-char-p (peek-char t))
      (num) (name accept)))

(defun range-end (names)
  (match-char #\-)
  (term names))

(defun range (min max names)
  (let ((lo (term names)) hi)
    (case (peek-char t)
	  ((#\-) (setq hi (range-end names)))
	  (t     (setq hi lo)))
    (cons lo hi)))

(defun field-list (min max names)
  (match-char #\,)
  (field min max names))

(defun wildcard (min max)
  (match-char #\*)
  (list (cons min max)))

(defun range-field (min max names)
  (list* (range min max names)
	 (case (peek-char t)
	       ((#\,) (field-list min max names))
	       (t     ()))))

(defun field (min max &optional names)
  (case (peek-char t)
	((#\*) (wildcard min max))
	(t     (range-field min max names))))

#+debug
(with-input-from-string
 (*standard-input* "30 6-8 3,4-5,9 Feb,3 Mon,Wed,Fri Comment")
 (cron-line))

(defun cron-tab ()
  "Parses a cron file."
  (case (peek-char t nil nil :end)
	((:end) ())
	((#\#)  (read-line) (cron-tab))
	(t      (cons (cron-line) (cron-tab)))))

#+debug
(with-input-from-string
 (*standard-input* "30 6-8 3,4-5,9 Feb,3 Mon,Wed,Fri Comment
# This is a comment line
   # so is this
0 7 2 10 * Lane swim")
 (cron-tab))
