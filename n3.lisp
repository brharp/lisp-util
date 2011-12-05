(defun rdf-n3-write-triples (&optional (stream *standard-output*) (db *rdf-db*))
  (dolist (x db)
    (rdf-write-triple x stream)))

(defun rdf-n3-write-triple (x stream)
  (rdf-n3-write (subject x) stream)
  (write-char #\Space stream)
  (rdf-n3-write (predicate x) stream)
  (write-char #\Space stream)
  (rdf-n3-write (object x) stream)
  (write-char #\Space stream)
  (when (graph x)
    (rdf-n3-write (graph x) stream)
    (write-char #\Space stream))
  (write-char #\. stream)
  (terpri stream))

(defun rdf-n3-write (object stream)
  (cond ((listp object)   (rdf-n3-write-triple object stream))
        ((url-p object)   (format stream "<~a>" (url-path object)))
        ((numberp object) (format stream "_:a~a" object))
        ((stringp object) (format stream "\"~a\"" object))))

#+nil
(dolist (x (subseq *rdf-db* 0 10))
  (rdf-n3-write-triple x *standard-output*))

(defun rdf-n3-triple ()
  (ecase (peek-char t)
    (#\< (url (rdf-n3-url)))
    (#\_ (rdf-n3-node-id))
    (#\" (rdf-n3-string))))

(defun rdf-n3-url ()
  (match-char #\<)
  (do ((char (read-char) (read-char))
       (buffer (make-array 1024 :fill-pointer 0)))
      ((not (char= #\> char))
       (coerce buffer 'string))
      (vector-push char buffer)))

(defun rdf-n3-node-id ()
  (match-char #\_)
  (match-char #\:)
  (xml-name))

(defun rdf-save (&optional (pathname "rdf.db") (db *rdf-db*))
  (with-open-file 
   (stream (merge-pathnames pathname) 
           :if-does-not-exist :create
           :if-exists :overwrite
           :direction :output)
   (rdf-n3-write-triples db stream)
   t))

(defun rdf-load (&optional (pathname "rdf.db"))
  (with-open-file (stream (merge-pathnames pathname) 
                     :direction :input)
     (setq *rdf-db* (read stream))
     t))
