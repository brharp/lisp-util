
(defvar *db-pathname-defaults* 
  "/var/local/db/*")

(defvar *db-directory*
  (merge-pathnames "default" *db-pathname-defaults*))

(defun db-open (name)
  (setf *db-directory*
    (merge-pathnames name *db-pathname-defaults*)))

(defun db-hash (key)
  (mod (sxhash key) 1024))

(defun db-hash-address (key)
  (merge-pathnames (format nil "~X" (db-hash key)) *db-directory*))

(defun db-put (key value)
  (let ((path (db-hash-address key)))
    (with-open-file (stream path :direction :io 
                            :element-type 'character
                            :if-does-not-exist :create)
      (let ((bucket (read stream nil nil)))
        (file-position stream 0)
        (prin1 (acons key value (remove key bucket :key #'car
                                        :test #'equal))
               stream)))))

(defun db-get (key)
  (let ((path (db-hash-address key)))
    (with-open-file (stream path :direction :input :element-type 'character
                            :if-does-not-exist :create)
      (let ((bucket (read stream nil nil)))
        (cdr (find key bucket :key #'car :test #'equal))))))

(defun db-map (function)
  (dolist (path (directory *db-directory*))
    (with-open-file (stream path :direction :input :element-type 'character)
      (let ((bucket (read stream nil nil)))
        (dolist (record bucket)
          (funcall function (car record) (cdr record)))))))





(provide "db")
