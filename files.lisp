;; -*- Mode: Lisp -*-
;; 
;; Functions for working with files.
;; 

(defun archive-directory-pathname (pathspec)
  (multiple-value-bind
   (sec min hour day month year)
   (decode-universal-time (file-write-date pathspec))
   (make-pathname
    :directory (append (or (pathname-directory pathspec) '(:relative))
                       (list (format nil "~a" year))))))

(defun archive-file-name (pathspec)
  (merge-pathnames (file-namestring pathspec)
                   (archive-directory-pathname pathspec)))

(defun archive-file (pathspec)
  (let ((new-path (archive-file-name pathspec)))
    (rename-file pathspec (ensure-directories-exist new-path))))

(defun now () (get-universal-time))

(defun modified-since-p (date pathspec)
  (> (file-write-date pathspec) date))

(defun directory-p (pathspec)
  (null (pathname-name pathspec)))

(defun auto-archive (&optional (path "*.*") (days 90))
  (let* ((date (- (now) (* days 24 60 60)))
         (test (lambda (x) (or (modified-since-p date x)
                               (directory-p x)))))
    (mapcar #'archive-file (remove-if test (directory path)))))
