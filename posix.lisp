
(defun popen (command mode)
  #+clisp
  (ext:run-shell-command (format nil "~A~{ '~A'~}" (first command) (rest command))
                         :input  (if (find #\w mode) :stream)
                         :output (if (find #\r mode) :stream))
  #+ecl
  (ext:run-program (first command) (rest command) :wait nil
                   :input  (if (find #\w mode) :stream)
                   :output (if (find #\r mode) :stream)
                   :error nil))

(defun get-env (name)
  (cadr (assoc name (environ) :test #'equal)))

(defvar *environ* nil)

(defun environ ()
  (unless *environ*
    (let ((stream (popen '("/usr/bin/env") "r")))
      (loop for line = (read-line stream nil nil)
            do (push (strtok "=" line 2) *environ*)
            while line)))
  *environ*)

(defun mktemp ()
  (let ((stream (popen '("/usr/bin/mktemp") "r")))
    (read-line stream)))

(defun strtok (sep string &optional (count 0))
  (cond ((zerop (length string)) ())
        ((zerop (1- count)) (list string))
        (t (let ((pos (position-if (lambda (char) (find char sep)) string)))
             (if (null pos) (list string)
               (cons (subseq string 0 pos)
                     (strtok sep (subseq string (1+ pos)) (1- count))))))))

(defun system (command)
  #+clisp (ext:run-shell-command command))


(provide "posix")
