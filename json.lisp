;;; JSON processing
;;; Copyright (c) 2015 M. Brent Harp

(defun json-match-char (char)
  (let ((inch (read-char)))
    (if (char= char inch) char
      (error "char mismatch: expected ~s but got ~s"
             char inch))))

(defun json-object ()
  (json-match-char #\{)
  (json-property-list))

(defun json-property-list ()
  (case (peek-char t)
    (#\} (read-char) ())
    (t   (append (json-property)
		 (json-property-list-rest)))))

(defun json-property-list-rest ()
  (case (peek-char t)
    (#\} (read-char) ())
    (t   (json-match-char #\,)
	 (json-property-list))))

(defun json-property ()
  (list (json-property-key)
        (json-property-value)))

(defun json-property-key ()
  (intern (string-upcase (json-string)) :keyword))

(defvar json-string-delim #\")

(defun json-string ()
  (json-match-char json-string-delim)
  (json-read-delimited-string json-string-delim))

(defun json-read-delimited-string (delim)
  (do ((char (read-char) (read-char))
       (buffer (make-array 1024 :fill-pointer 0)))
      ((eq delim char)
       (coerce buffer 'string))
      (vector-push char buffer)))

(defun json-property-value ()
  (json-match-char #\:)
  (json-value))

(defun json-number-char-p (char)
  (or (char= #\+ char)
      (char= #\- char)
      (char= #\. char)
      (char= #\e char)
      (char= #\E char)
      (digit-char-p char)))

(defun json-number ()
  (do ((char (read-char) (read-char nil nil))
       (buffer (make-array 64 :fill-pointer 0)))
      ((or (null char) (not (json-number-char-p char)))
       (when char (unread-char char))
       (read-from-string (coerce buffer 'string)))
      (vector-push char buffer)))

(defun json-array ()
  (json-match-char #\[)
  (json-value-array))

(defun json-value-array ()
  (case (peek-char t)
    (#\] (read-char) ())
    (t   (cons (json-value)
	       (case (read-char)
		 (#\, (json-value-array))
		 (#\] ()))))))

(defun json-value ()
  (case (peek-char t)
    (#\{  (json-object))
    (#\[  (json-array))
    (#\"  (json-string))
    (t    (json-number))))

#+debug
(with-input-from-string
 (*standard-input* "{\"x\": 42, \"y\": 7}")
 (json-value))

#+debug
(with-input-from-string
 (*standard-input* "{\"x\": 42}")
 (json-value))

#+debug
(with-input-from-string
 (*standard-input* "[\"x\", 42, \{\"foo\": \"bar\"\}]")
 (json-value))

(defun json-object-p (value)
  (and (consp value) (keywordp (car value))))

(defun json-array-p (value)
  (and (consp value) (not (keywordp (car value)))))

(defun json-print-property-list (value)
  (when value
    (json-print-property-key (car value))
    (write-char #\:)
    (write-char #\Space)
    (json-print (cadr value))
    (json-print-property-list-rest (cddr value))))

(defun json-print-property-list-rest (value)
  (when value
    (write-char #\Space)
    (json-print-property-list value)))

(defun json-print-property-key (value)
  (write-char #\")
  (write-string (symbol-name value))
  (write-char #\"))

(defun json-print-object (value)
  (prog1 value
    (write-char #\{)
    (json-print-property-list value)
    (write-char #\})))

(defun json-print-number (value)
  (write value))

(defun json-print-string (value)
  (prog1 value
    (write-char #\")
    (write-string value)
    (write-char #\")))

(defun json-print (value)
  (cond ((json-object-p value) (json-print-object value))
	((json-array-p value)  (json-print-array value))
	((stringp value)       (json-print-string value))
	(t                     (json-print-number value))))

(provide "json")
