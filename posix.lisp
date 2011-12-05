#+clisp
(defun popen (command mode)
  (ext:run-shell-command (format nil "~A~{ ~A~}" (first command) (rest command))
                         :input  (if (find #\w mode) :stream)
                         :output (if (find #\r mode) :stream)))

#+ecl
(defun popen (command mode)
  (ext:run-program (first command) (rest command) :wait nil
                   :input  (if (find #\w mode) :stream)
                   :output (if (find #\r mode) :stream)
                   :error nil))

(provide "posix")
