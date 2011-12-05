;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; I/O Functions
;;; Copyright 2010 M. Brent Harp

;; Filter Streams

(defclass filter-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defmethod stream-element-type ((stream filter-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream filter-stream) &key abort)
  (close (stream-of stream) :abort abort))




(defclass filter-character-input-stream
  (filter-stream fundamental-character-input-stream)
  ())

(defmethod stream-read-char ((stream filter-character-input-stream))
  (read-char (stream-of stream) nil :eof))

(defmethod stream-unread-char ((stream filter-character-input-stream)
                               char)
  (unread-char char (stream-of stream)))





(defclass pushback-character-input-stream
  (filter-character-input-stream)
  ((buffer :reader buffer-of)))

(defmethod initialize-instance
  ((instance pushback-character-input-stream)
   &key (size 1) &allow-other-keys)
  (call-next-method)
  (setf (slot-value instance 'buffer)
        (make-array size :fill-pointer 0)))

(defmethod stream-read-char ((stream pushback-character-input-stream))
  (with-accessors ((buf buffer-of)) 
      stream
    (if (plusp (fill-pointer buf))
      (vector-pop buf)
      (call-next-method))))

(defmethod stream-unread-char ((stream pushback-character-input-stream)
                               char)
  (vector-push char (buffer-of stream)))

(defmethod stream-peek-char
  ((stream pushback-character-input-stream))
  (with-accessors ((buf buffer-of)) 
      stream
    (if (plusp (fill-pointer buf))
      (aref buf (1- (fill-pointer buf)))
      (call-next-method))))


;(eval-when (:load-toplevel)
;  (let ((s (make-instance 'pushback-character-input-stream
;                          :stream (make-string-input-stream "aaabbbcc")
;                          :size 3)))
;   (read-char s)
;   (read-char s)
;   (read-char s)
;   (unread-char #\a s)
;   (unread-char #\a s)
;   (unread-char #\a s)
;   (read-char s)
;   (assert
;    (eq (read s) 'aabbcc))))



;;; This is an adaptation of Oleg's input-parse.scm, for Lisp. See
;;; http://okmij.org/ftp/Scheme/parsing.html.
;;; 

(defun assert-curr-char (char-list &optional (stream *standard-input*)
				   (eof-error-p t) (eof-value nil))
  (let ((c (read-char stream eof-error-p eof-value)))
    (if (member c char-list)
	c
      (error "Expected ~A." char-list))))


(defun skip-until (char-list &optional (stream *standard-input*)
			     (eof-error-p t) (eof-value nil))
  (if (numberp char-list)
      ;; skip-until count
      (do ((i 1 (+ i 1)))
	  ((>= i char-list))
	  (read-char stream eof-error-p eof-value))
    ;; skip-until char-list
    (let ()
      ;; If EOF is not an error condition, then eof-value must be
      ;; recognized as a break character to avoid an infinite loop.
      (if (not eof-error-p) (setf char-list (cons eof-value char-list)))
      (loop
       (let ((c (read-char stream eof-error-p eof-value)))
	 (if (member c char-list)
	     (return c)))))))


(defun skip-while (char-list &optional (stream *standard-input*)
			     (eof-error-p t) (eof-value nil))
  ;; If EOF is not an error condition, then eof-value must be
  ;; recognized as a break character to avoid looping indefinitely.
  (if eof-error-p (push eof-value char-list))
  (loop
   (let ((c (peek-char nil stream eof-error-p eof-value)))
     (if (not (member c char-list))
	 (return c)
       (read-char stream)))))


(let ((b (make-string 1024)))
  (defun init-buffer ()
    b))

(defun next-token (prefix-char-list break-char-list 
                   &optional (stream *standard-input*) (eof-error-p t) 
                   (eof-value nil))
  (skip-while prefix-char-list stream eof-error-p eof-value)
  (if (not eof-error-p) (push eof-value break-char-list))
  (let ((buffer (init-buffer))
	(buffer-list '()))
    (do ((i 0 (mod (+ i 1) (length buffer)))
	 (c (peek-char nil stream eof-error-p eof-value)
	    (peek-char nil stream eof-error-p eof-value)))
	((member c break-char-list)
	 (if (and (null buffer-list) (zerop i))
	     eof-value
	   (apply (args #'concatenate 'string)
		  (reverse (cons (subseq buffer 0 i) buffer-list)))))
      (if (>= i (length buffer))
	  (let ()
	    (push buffer buffer-list)
	    (setf buffer (make-string 1024))))
      (setf (aref buffer i) (read-char stream nil nil)))))


(defun next-token-of (charset/pred &optional (stream *standard-input*)
				   (eof-error-p t) (eof-value nil))
  (cond
   ((consp charset/pred) (next-token-of (lambda (c)
					  (member c charset/pred))
					stream eof-error-p eof-value))
   ((functionp charset/pred)
    (do ((token nil)
	 (c (peek-char nil stream eof-error-p eof-value)
	    (peek-char nil stream eof-error-p eof-value)))
	((not (funcall charset/pred c))
	 (coerce (reverse token) 'string))
	(push (read-char stream eof-error-p eof-value) token)))
    (t
     (error "next-token-of: expected cons or function but got ~A instead."
	    charset/pred))))





;; How I write parsers:
;; - one function per non-terminal (production)
;; - factor out lookahead ambiguity
;; - productions take (stream char)
;; - choose between productions with (case char ...)

;; Passing a character as a second argument to every reader function
;; reduces the amount of peeking and unreading. It also makes it easy
;; to install productions as reader macros. To invoke a reader on a
;; null stream, prime by calling (read-char stream).

(defun parse (what &optional (stream *standard-input*)
                   (eof-error-p t) (eof-value nil))
  (funcall (intern (concatenate 'string (string 'read-) (string what)))
           stream (read-char stream eof-error-p eof-value)))

;; Parsers for common objects follow.

(defun char-digit (c)
  "Returns the numeric value of a decimal digit."
  (declare (type character c))
  (- (char-code c) #.(char-code #\0)))

;; integer := <DIGIT>+
(defun read-integer (stream char)
  (do ((c char (read-char stream nil nil))
       (n 0    (+ (* 10 n) (char-digit c))))
      ((or (null c) (not (digit-char-p c)))
       (when c (unread-char c stream))  ;cleanup
       n)))

    
(defun read-string (stream char)
  "Naive string reader."
  (let ((buffer (make-array 256 :fill-pointer 0 :element-type 'character)))
    (do ((c (read-char stream t) (read-char stream t)))
        ((eq c char) (subseq buffer 0 (length buffer)))
    (vector-push c buffer))))

(assert
 (string= "hello"
          (with-input-from-string
           (s "\"hello\"")
           (parse 'string s))))

;; range-rest := "," range | nil
(defun read-range-rest (stream char)
  (case char
    (#\, (read-range stream (read-char stream)))
    (t   (unread-char char stream))))
  
;; interval-rest := "-" integer | nil
(defun read-interval-rest (stream char)
  (case char
    (#\- (read-integer stream (read-char stream)))
    (t   (unread-char char stream))))

;; interval := integer interval-rest
(defun read-interval (stream char)
  (let ((start (read-integer stream char))
        (end   (read-interval-rest stream (read-char stream nil))))
    (range start (or end start))))

;; range := interval range-rest
(defun read-range (stream char)
  (range+ (read-interval stream char)
          (read-range-rest stream (read-char stream nil))))

;(assert
 ;(range= (range 10 20)
         ;(with-input-from-string 
          ;(s "10-20")
          ;(parse 'range s))))

;(assert
 ;(range= (range 10 20 30 40)
         ;(with-input-from-string 
          ;(s "10-20,30-40")
          ;(read-range s (read-char s)))))

;(assert
 ;(range= (range 10 40)
         ;(with-input-from-string 
          ;(s "10-30,20-40")
          ;(read-range s (read-char s)))))

;(assert
 ;(range= (range 10 40)
         ;(with-input-from-string 
          ;(s "20-40,10-30")
          ;(read-range s (read-char s)))))




(defun skip-char (&optional skip-type (stream *standard-input*)
                            (eof-error-p t) (eof-value nil))
  "skip-char is used to skip over characters in an input stream. It
returns the stream.

If skip-type is nil or not specified, skip-char skips a single
character. If skip-type is t, skip-char skips over whitespace
characters, leaving the first non-whitespace character unread. If
skip-type is a character, skip-char skips over characters that are not
char= to skip-type."
  (prog1 stream
    (cond
     ((null skip-type) (read-char stream eof-error-p eof-value))
     ((eq t skip-type) (peek-char t stream eof-error-p eof-value))
     (t (do ((c (read-char stream eof-error-p eof-value)
                (read-char stream eof-error-p eof-value)))
            ((not (char= c (the character skip-type)))
             (unread-char c stream)))))))

(assert
 (char= #\H
   (with-input-from-string (s "     Hello")
     (read-char (skip-char t s)))))

(with-input-from-string (s "     Hello")
  (read-char (skip-char nil s)))
(with-input-from-string (s "     Hello")
  (read-char (skip-char #\Space s)))


