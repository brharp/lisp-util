(require "xml")
(require "ical")

(defun date (x)
  (first x))

(defun program (x)
  (second x))

(defun category (x)
  (third x))

(defun dur (x)
  (fourth x))

(defun group-by (x)
  (cons (date x) (dur x)))

(defun map-reduce (fn key lst)
  (let ((h (make-hash-table)) (result ()))
    (dolist (x (mapcar key lst)) (push (cdr x) (gethash (car x) h)))
    (maphash (lambda (key lst) (push (cons key (reduce fn lst)) result)) h)
    result))

(defun token ()
  (do ((char (read-char nil nil) (read-char nil nil))
       (buffer (make-array 256 :fill-pointer 0)))
      ((or (null char) (not (alphanumericp char)))
       (coerce buffer 'string))
      (vector-push char buffer)))

(defun tokens (string)
  (when (plusp (length string))
    (let ((sep (position-if-not #'alphanumericp string)))
      (if sep (cons (subseq string 0 sep) (tokens (subseq string (1+ sep))))
        (list string)))))

(defun date (time)
  (fourth (multiple-value-list (decode-universal-time time))))

(defun load-data ()
  (with-open-file 
   (*standard-input* "test.ics")
   (let* ((calendar (ical-object))
          (events   (xml-get-elements-by-tag-name calendar "VEVENT")))
     (dolist (event events)
       (let* ((dtstart  (first (xml-get-elements-by-tag-name event "DTSTART")))
              (dtend    (first (xml-get-elements-by-tag-name event "DTEND")))
              (summary  (first (xml-get-elements-by-tag-name event "SUMMARY")))
              (rrule    (first (xml-get-elements-by-tag-name event "RRULE")))
              (start    (ical-parse-date (xml-first-child dtstart)))
              (end      (ical-parse-date (xml-first-child dtend)))
              (recur    (ical-parse-recur (xml-first-child rrule)))
              (recid    (ical-apply-rrule start recur))
              (durmin   (floor (/ (- end start) 60)))
              (sumtxt   (xml-first-child summary))
              (tokens   (tokens sumtxt))
              (program  (first tokens))
              (category (second tokens)))
         (dolist (time recid)
           (push (list (date time) program category durmin) data)))))))
  
(defun filter (x lst &key key)
  (remove-if-not (lambda (y) (eq x (funcall key y))) lst))

(defvar programs
  '((ip ax tx cx)
    (op ax tx cx)))

(defun report ()
  (dolist (program programs)
    (let ((program-code (first program))
          (program-categories (rest program)))
      (format t "~%~a ---------------~%" program-code)
      (let ((program-data (filter program-code data :key #'program)))
        (dolist (category-code program-categories)
          (print category-code)
          (let ((category-data (filter category-code program-data :key #'category)))
            (print (sort (map-reduce #'+ #'group-by category-data) #'< :key #'car))))
        (print 'TOTAL)
        (print (sort (map-reduce #'+ #'group-by program-data) #'< :key #'car))))))

(report)



