
(defvar *temporary-package-list* ())

(defun use (package-name &key (as (package-name *package*)))
  "USE loads the module named by PACKAGE-NAME."
  (import-external-symbols-from
   (or (find-package (string-upcase package-name))
       (load-package package-name)
       (return-from use nil))
   (or (find-package (string-upcase as))
       (make-temporary-package (string-upcase as)))))

(defun make-temporary-package (name)
  (let ((package (make-package name)))
    (push package *temporary-package-list*)
    package))

(defun import-external-symbols-from (from-package to-package)
  (do-symbols (sym from-package)
    (shadowing-import sym to-package))
  (do-external-symbols (sym from-package)
    (export sym to-package))
  t)

(defun load-package (name)
  (handler-case (let ((*package* (make-package (string-upcase name))))
                  (use-package 'common-lisp)
                  (import      'use)
                  (load-package-file name)
                  *package*)
     (error (e)
       (delete-package (string-upcase name))
       (error e))))

(defun load-package-file (name)
  (let ((*temporary-package-list* ()))
    (unwind-protect 
        (load name)
      (map nil #'delete-package *temporary-package-list*))))


