;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; MySQL Interface
;;; Copyright (c) 2022 M. Brent Harp

(eval-when
 (:compile-toplevel)
 (load "config.lisp"))

(require #:xml   #.(translate-logical-pathname "sys:lib;xml"))
(require #:posix #.(translate-logical-pathname "sys:lib;posix"))

(defvar *mysql-database*)
(defvar *mysql-command* "mysql")
(defvar *mysql-result*)

(defun mysql (database execute)
  (let ((cmd (list *mysql-command* (or database *mysql-database*) "-X")))
    (when execute (setq cmd (append cmd (list "-e" execute))))
    (let ((*standard-input* (popen cmd "r")))
      (setq *mysql-result* (xml-document))
      (close *standard-input*)
      *mysql-result*)))

(provide #:mysql)
