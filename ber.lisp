;; -*- mode: lisp -*-

(defun encode-id (id)
  (list id))

(defun encode-length (length)
  (list length))

(defun encode-id-length-content (id length content)
  (append (encode-id id) (encode-length length) content))

(defun encode-id-content (id content)
  (encode-id-length-content id (length content) content))

(defun encode-list (lst)
  (dolist (x lst)
    (write-byte x *standard-output*)))

(defgeneric object-id (obj))

(defgeneric object-content (obj))

(defgeneric encode-object (obj))

(defmethod encode-object (obj buffer start)
  (reduce #'append
          (map #'encode-object (get-version obj) buffer)
       
  (encode-id (object-id obj) buffer start)
  (encode-length (
  (encode-id-content (object-id obj) (object-content obj)))

(defun universal (id)
  id)

(defun mkel (id length content)
  (list id length content))

(defmethod encode-object ((n integer))
  (let* ((len (ceiling (/ (1+ (integer-length n)) 8)))
         (con (make-array len :fill-pointer 0)))
    (do ((n n (ash n -8)) (i len (1- i)))
        ((zerop i)) (vector-push (ldb (byte 8 0) n) con))
    (mkel (universal 2) len con)))

;(encode-object 32)

(defmethod encode-object ((s string))
  (encode-id-length-content
   (universal 4) (length s)
   (map 'list #'char-code s)))

;(encode-object "foobar")

(defmethod object-id ((object cons))
  (declare (ignore object))
  (universal 48))

(defmethod object-content ((object cons))
  (reduce (function append)
          (mapcar (function encode-object)
                  object)))

;(encode-object '(3 "brharp" "password"))

(defclass bind-request ()
  ((version :reader get-version :initarg :version)
   (name    :reader get-name    :initarg :name)
   (auth    :reader get-auth    :initarg :auth)))

(defun application (tag)
  (logior #x60 tag))

(defmethod object-id ((obj bind-request))
  (declare (ignore obj))
  (application 0))

(defmethod object-content ((obj bind-request))
  (object-content
   (list (get-version obj)
         (get-name obj)
         (get-auth obj))))


(defun decode-id (lst)
  lst)

(defun decode-length (lst)
  lst)

(defvar *decoder-table* (make-hash-table))

(defun set-decoder (id function)
  (setf (gethash id *decoder-table*) function))

(defun get-decoder (id)
  (or (gethash id *decoder-table*)
      (error "No decoder for ~A" id)))

(defun decode-object (lst)
  (let* ((x (decode-id lst))
         (id (car x))
         (y (decode-length (cdr x)))
         (length (car y))
         (content (cdr y)))
    (funcall (get-decoder id)
             id length content)))

       
(defun decode-integer (id length content)
  (declare (ignore id))
  (do ((n 0 (setf (ldb (byte 8 0) n) (car c)))
       (c content (cdr c))
       (i length (1- i)))
      ((zerop i) (cons n c))))

(set-decoder (universal 2) (function decode-integer))
;(encode-object 77)
;(decode-list *)
 

(defun decode-string (id length content)
  (declare (ignore id))
  (cons (coerce (mapcar (function code-char)
                        (subseq content 0 length))
                'string)
        (nthcdr length content)))

(set-decoder (universal 4) (function decode-string))
;(encode-object "foobar")
;(decode-list *)
    
(defun decode-bind-request (id length content)
  (declare (ignore id length))
  (let* ((version (decode-object content))
         (name    (decode-object (cdr version)))
         (auth    (decode-object (cdr name))))
    (make-instance 'bind-request
                   :version (car version)
                   :name    (car name)
                   :auth    (car auth))))

(set-decoder (application 0) (function decode-bind-request))

;(make-instance 'bind-request :version 3 :name "brharp" :auth "password")
;(encode-object *)
;(decode-object *)


(defstruct (bind-request (:type list) :named)
  version name auth)
