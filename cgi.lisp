;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; CGI
;;; Copyright (c) 2011 M. Brent Harp

(require "posix")

(defvar *cgi-query-string* nil)
(defvar *cgi-query-alist* nil)
(defvar *cgi-cookie-string* nil)
(defvar *cgi-cookie-alist* nil)

(defun cgi-get-query (name)
  (cgi-url-decode (cdr (assoc name (cgi-get-query-alist) :test #'equal))))

(defun cgi-get-query-alist ()
  (or *cgi-query-alist*
      (setq *cgi-query-alist*
            (cgi-parse-query-string (cgi-get-query-string)))))

(defun cgi-get-query-plist ()
  (apply #'nconc 
         (mapcar (lambda (a)
                   (list (intern (string-upcase (car a)) "KEYWORD")
                         (cgi-url-decode (cdr a))))
                 (cgi-get-query-alist))))

(defun cgi-get-query-string ()
  (or *cgi-query-string*
      (setq *cgi-query-string*
            (get-env "QUERY_STRING"))))

(defun cgi-parse-query-string (string)
  (if (zerop (length string)) ()
    (let ((amp (position #\& string)))
      (if (null amp) (list (cgi-parse-nv-pair string))
        (cons (cgi-parse-nv-pair (subseq string 0 amp))
              (cgi-parse-query-string (subseq string (1+ amp))))))))

(defun cgi-parse-nv-pair (string)
  (if (zerop (length string)) ()
    (let ((eq (position #\= string)))
      (if (null eq) (cons string string)
        (cons (intern (string-upcase (cgi-url-decode (subseq string 0 eq))))
              (cgi-url-decode (subseq string (1+ eq))))))))

(defun cgi-url-decode (string &optional start end)
  (with-output-to-string (output)
    (do* ((i (or start 0) (1+ i)))
         ((>= i (or end (length string))))
         (let ((c (elt string i)))
           (cond 
            ((char= #\% c)
             (let ((n (parse-integer string :start (1+ i) :end (+ i 3) :radix 16)))
               (write-char (code-char n) output)
               (incf i 2)))
            ((char= #\+ c)
             (write-char #\Space output))
            (t
             (write-char c output)))))))

(defun cgi-url-encode (string &optional start end)
  (with-output-to-string (output)
    (do* ((i (or start 0) (1+ i)))
         ((>= i (or end (length string))))
         (let ((c (elt string i)))
           (cond 
            ((not (alphanumericp c))
             (format output "%~X" (char-code c)))
            (t
             (write-char c output)))))))

(defun cgi-set-cookie (name value)
  (format t "Set-Cookie: ~A=~A~%" name value))

(defun cgi-get-cookie (name)
  (cadr (assoc name (cgi-get-cookie-alist) :test #'equal)))

(defun cgi-get-cookie-alist ()
  (or *cgi-cookie-alist*
      (setq *cgi-cookie-alist*
            (cgi-parse-cookie-string (cgi-get-cookie-string)))))

(defun cgi-get-cookie-string ()
  (or *cgi-cookie-string*
      (setq *cgi-cookie-string*
            (get-env "HTTP_COOKIE"))))

(defun cgi-split-string (delim string)
  (if (zerop (length string)) ()
    (let ((pos (position delim string)))
      (if (null pos) (list string)
        (cons (subseq string 0 pos)
              (cgi-split-string delim (subseq string (1+ pos))))))))

(defun cgi-parse-cookie-string (string)
  (mapcar (lambda (s) (cgi-split-string #\= s))
          (cgi-split-string #\; string)))

(defun cgi-send-redirect (url)
  (format t "Location: ~A~%~%" url))

(defun cgi-self ()
  (format nil "~A?~A" (get-env "SCRIPT_NAME") (get-env "QUERY_STRING")))

(defun cgi-path-info ()
  (get-env "PATH_INFO"))

(defun cgi-script-name ()
  (get-env "SCRIPT_NAME"))

(defun cgi-query-string ()
  (get-env "QUERY_STRING"))

(defun cgi-server-name ()
  (get-env "SERVER_NAME"))

(defun cgi-server-protocol ()
  (get-env "SERVER_PROTOCOL"))
