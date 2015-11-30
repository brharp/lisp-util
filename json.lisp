;;; JSON processing
;;; Copyright (c) 2015 M. Brent Harp

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (load "config.lisp"))

(defun match-char (char)
  (let ((inch (read-char)))
    (if (char= char inch) char
      (error "char mismatch: expected ~s but got ~s"
             char inch))))

(defun json-object ()
  (match-char #\{)
  (json-property-list))

(defun json-property-list ()
  (case (peek-char t)
    (#\} (read-char) ())
    (t   (cons (json-property) (json-property-list)))))

(defun json-property ()
  (cons (json-property-key)
        (json-property-value)))

(defun json-property-key ()
  (json-string))
  
(defvar json-string-delim #\")

(defun json-string ()
  (match-char json-string-delim)
  (read-delimited-string json-string-delim))

(defun read-delimited-string (delim)
  (do ((char (read-char) (read-char))
       (buffer (make-array 1024 :fill-pointer 0)))
      ((eq delim char)
       (coerce buffer 'string))
      (vector-push char buffer)))

(defun json-property-value ()
  (match-char #\:)
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
  (match-char #\[)
  (cons 'json-array (json-value-array)))

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

