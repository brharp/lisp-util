(load "../json")
(in-package :cl-user)

(defpackage #:json-test
  (:use #:json #:common-lisp)
  (:import-from #:json #:json-number #:json-array #:json-object)
  )

(in-package #:json-test)

(defun run-test (test)
  (let ((exp (copy-tree (first test)))
	(input (getf (rest test) ':input))
	(expec (getf (rest test) '=> ':anything))
	(result nil))
    (if input
      (with-input-from-string (*standard-input* input)
	(setf result (eval exp)))
      (setf result (eval exp)))
    (unless (or (equal expec ':anything)
		(equal expec result))
      (format *terminal-io*
	      "~%**** FAILED: ~S~%  expected: ~S~%  got: ~S~%"
	      exp expec result))))

(run-test '((json-number) => 123 :input "123"))

(with-input-from-string
 (*standard-input* "-123.0")
 (assert (= (json-number) -123)))

(with-input-from-string
 (*standard-input* "0")
 (assert (equal (json-number) 0)))

(with-input-from-string
 (*standard-input* "")
 (assert
  (handler-case 
   (json-number)
   (condition () t)
   )))

(with-input-from-string
    (*standard-input* "[ \"foo\", \"bar\", 42 ]")
  (assert (equal (json-array) (list 'json-array "foo" "bar" 42))))

(with-input-from-string
    (*standard-input* "[ ]")
  (assert (equal (json-array) (list 'json-array))))

(with-input-from-string
    (*standard-input* "[")
  (assert
   (handler-case 
       (and (json-array) nil)
     (condition () t))))

