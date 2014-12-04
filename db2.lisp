
(defconstant db-page-size 8192)


(defstruct db-page
  (records nil)
  (parent nil)
  (next nil))


(defstruct db-record
  (key nil)
  (value nil)
  (pointer nil))


(defun db-init (path)
  (with-open-file (file path :direction :output)
    (db-write-page (make-db-page) 0 file))
  path)


(defun db-create (path)
  (db-init path)
  (db-open path))

        
(defun db-open (path)
  (cond ((not (probe-file path)) (db-create path))
        (t (open path :direction :io :if-exists :overwrite))))


(defconstant db-blank-page
  (make-string db-page-size :initial-element #\Space))


(defun db-write-page (page address file)
  (let ((*print-pretty* nil))
    (file-position file address)
    (princ db-blank-page file)
    (file-position file address)
    (prin1 page file)))


(defun db-read-page (address file)
  (when (file-position file address)
    (read file)))


(defun db-insert (record address file)
  "Inserts a record into a page. If the resulting page would be full,
then the page is split.

The page at `address' is read into memory, and the new record
inserted. Records are sorted by key. If the new page size exceeds the
maximum page size, then the page is split, producing a new parent. The
record is reinserted into the new parent page. If the new record fits
on the existing page, no splitting occurs and the page is written back
to disk."
  (let ((page (db-read-page address file)))
    (setf (db-page-records page)
          (sort (cons record (delete (db-record-key record) 
                                     (db-page-records page)
                                     :test #'string=
                                     :key #'db-record-key))
                #'string< :key #'db-record-key))
    (let ((string (prin1-to-string page)))
      (cond ((> (file-string-length file string) db-page-size)
             (let ((parent (db-split-page address file)))
               (multiple-value-bind (exists address)
                   (db-search (db-record-key record) parent file)
                 (db-insert record address file))))
            (t ; does not overflow page size
             (db-write-page page address file)
             address)))))


(defun db-split-page (address file)
  (let* ((page     (db-read-page address file))
         (parent   (db-page-parent page))
         (records  (db-page-records page))
         (left     (file-length file))
         (right    (+ left db-page-size)))
    (cond ((null parent) ; split root
           (db-write-page (make-db-page :next right) 0 file)
           (db-write-page (make-db-page :parent 0) left file)
           (db-write-page (make-db-page :parent 0) right file)
           (db-distribute-keys records 0 left right file))
          (t ; split internal node
           (db-write-page (make-db-page :parent parent) left file)
           (db-distribute-keys records parent left address file)))))


(defun db-distribute-keys (records parent left right file)
  (let* ((length        (length records))
         (middle        (floor (/ length 2)))
         (middle-record (elt records middle))
         (middle-key    (db-record-key middle-record)))
    (db-replace (remove middle-key records :test #'string<= :key #'db-record-key) left file)
    (db-replace (remove middle-key records :test #'string>= :key #'db-record-key) right file)
    (db-insert (make-db-record :key middle-key :pointer left) parent file)))


(defun db-replace (records address file)
  (let ((page (db-read-page address file)))
    (setf (db-page-records page) records)
    (db-write-page page address file)))


(defun db-search (key address file)
  (let* ((page    (db-read-page address file))
         (records (db-page-records page))
         (found   (find key records :key #'db-record-key :test #'string<=))
         (pointer (if found (db-record-pointer found)))
         (next    (db-page-next page)))
    (cond ((and (null found) (null next))
           (values nil address))
          ((null found)
           (db-search key next file))
          ((string= key (db-record-key found))
           (values found address))
          ((null pointer)
           (values nil address))
          (t ;otherwise, follow pointer
           (db-search key pointer file)))))


(defun db-put (key value file)
  (let ((record (make-db-record :key (princ-to-string key) :value value)))
    (multiple-value-bind (exists address)
        (db-search key 0 file)
      (db-insert record address file))))


(defun db-get (key file)
  (let ((found (db-search (princ-to-string key) 0 file)))
    (when found (db-record-value found))))


(defun db-lock-file-path (pathspec)
  (let ((pathname (pathname pathspec)))
    (make-pathname :host (pathname-host pathname)
                   :device (pathname-device pathname)
                   :directory (pathname-directory pathname)
                   :name (pathname-name pathname)
                   :type "lock"
                   :version (pathname-version pathname))))


(defun db-lock (path)
  (let ((lock-file-path (db-lock-file-path path)))
    (loop (let ((lock-file (open lock-file-path :direction :probe 
                                 :if-does-not-exist :create :if-exists nil)))
            (if lock-file (return lock-file) (sleep 1))))))


(defun db-unlock (path)
  (delete-file (db-lock-file-path path)))


(defmacro with-db-open ((db path) &body body)
  `(let ((,db (db-open ,path)))
     (unwind-protect (progn ,@body)
       (close ,db))))


(defmacro with-db-lock ((file) &body body)
  (let ((lock (gensym)))
    `(let ((,lock (db-lock (pathname ,file))))
       (unwind-protect (progn ,@body)
         (db-unlock (pathname ,file))))))


(defun db-map (function file)
  (db-map-page function 0 file))

(defun db-map-page (function address file)
  (let ((page (db-read-page address file)))
    (dolist (record (db-page-records page))
      (funcall function (db-record-key record)
               (db-record-value record))
      (when (db-record-pointer record)
        (db-map-page function (db-record-pointer record) file)))
    (when (db-page-next page)
      (db-map-page function (db-page-next page) file)))
  t)

      
#+debug
(with-db-open (db "lock.db")
  (with-db-lock (db)
    (db-put "foo" "bar" db)))


#+debug
(let* ((db (db-open "test.db")))
  (db-put 43 db)
  (db-put 74 db)
  (db-put 13 db)
  (db-put 31 db)
  (db-put 10 db)
  (db-put 70 db)
  (db-put 20 db)
  (db-put 90 db)
  (db-put 30 db)
  (db-put 50 db)
  (db-put 60 db)
  (db-put 40 db)
  (db-put 80 db)
  (db-put 68 db))


#+debug
(let ((db (db-open "test.db")))
  (db-put 55 "fifty-five" db))

#+debug
(let ((db (db-open "test.db")))
  (db-get 55 db))

(provide "db2")
