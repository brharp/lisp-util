
(defpackage :json
  (:use :common-lisp))

(in-package :json)

(defun match-char (char)
  (let ((inch (read-char)))
    (if (char= char inch) char
      (error "char mismatch: expected ~s but got ~s"
             char inch))))

(defun match-token-of
 (do ((char (read-char) (read-char))
       (buffer (make-array 256 :fill-pointer 0)))
      ((not (xml-name-char-p char))
       (progn (unread-char char)
              (coerce buffer 'string)))
      (vector-push char buffer)))


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
  (if (char= #\- (peek-char))
      (read-char)
(defun json-value ()
  (ecase (peek-char t)
    (#\{  (json-object))
    (#\[  (json-array))
    (#\"  (json-string))
    (t    (json-number))))
