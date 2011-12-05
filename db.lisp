(defun variablep (x)
  (and (symbolp x) (eq (elt (symbol-name x) 0) #\?)))

(defstruct db-tree
  (first nil) (rest nil) 
  (keys (make-hash-table)))

(defvar *db* (make-db-tree))

(defun db-put (key value &optional (db *db*))
  (cond ((consp key)
         (db-put (first key) value 
                 (or (db-tree-first db)
                     (setf (db-tree-first db) (make-db-tree))))
         (db-put (rest key) value
                 (or (db-tree-rest db)
                     (setf (db-tree-rest db) (make-db-tree)))))
        ((null key))
        (t (vector-push-extend
            value (or (gethash key (db-tree-keys db))
                      (setf (gethash key (db-tree-keys db))
                            (make-array 1 :adjustable t
                                        :fill-pointer 0)))))))

(defun db-get (key &optional (best-n most-positive-fixnum) 
                   (best-list (make-array 0)) (db *db*))
  (cond ((or (null key) (variablep key))
         (values best-list best-n))
        ((consp key)
         (multiple-value-bind (list n)
          (db-get (first key) best-n best-list (db-tree-first db))
          (db-get (rest key) n list (db-tree-rest db))))
        (t (let* ((list (gethash key (db-tree-keys db)))
                  (n (length list)))
             (if (< n best-n)
                 (values list n)
               (values best-list best-n))))))
 
(defun match (pattern input bindings)
  (cond ((eql bindings fail) fail)
        ((variable-p pattern)
         (let ((binding (assoc pattern bindings)))
           (cond ((null binding) (acons pattern input bindings))
                 ((eql (cdr binding) input) bindings)
                 (t fail))))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (match (rest pattern) (rest input)
                (match (first pattern) (first input) bindings)))
        (t fail)))
