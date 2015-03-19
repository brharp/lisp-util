
(eval-when (:compile-toplevel)
  (load "config"))

(require "posix" #.(translate-logical-pathname "config:lib;lisp;posix"))

(defvar *http-protocol* "http")
(defvar *http-host* "localhost")
(defvar *http-port* 80)
(defvar *http-header* "Content-Type: text/plain")

(defun http (path &key (body "") (protocol *http-protocol*)
             (host *http-host*) (port *http-port*) (header *http-header*) (debug nil))
  (when debug (format t "HTTP: DEBUG: BODY: ~s~%" body))
  (let ((url (format nil "~a://~a:~a~a" protocol host port path)))
    (popen (list "curl" url "--insecure" "--silent" "--header" header "--data-binary" body) "r")))

(provide "http")
