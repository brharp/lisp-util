
;; -------- Buffers -------- 

(defgeneric flip (obj))

(defclass buffer ()
  ((capacity :accessor capacity-of :initarg :capacity)
   (position :accessor position-of :initform 0)
   (limit    :accessor limit-of    :initform 0)
   (mark     :accessor mark-of     :initform 0)))

(defmethod flip ((obj buffer))
  (setf (limit-of obj) (position-of obj)
        (position-of obj) 0))
 
(defgeneric get-byte (obj &key position &allow-other-keys))
(defgeneric put-byte (obj value &key position &allow-other-keys))
(defgeneric get-integer (obj &key position &allow-other-keys))
(defgeneric put-integer (obj value &key position &allow-other-keys))
(defgeneric get-string (obj &key position &allow-other-keys))
(defgeneric put-string (obj value &key position &allow-other-keys))

;; -------- Byte Buffers -------- 

(defclass byte-buffer (buffer)
  ((source :reader source-of)))

(defmethod initialize-instance ((obj byte-buffer) &key capacity)
  (call-next-method obj)
  (setf (slot-value obj 'source)
        (make-array capacity :element-type 'unsigned-byte)))

(defmethod get-byte ((obj byte-buffer) &key position)
  (svref (source-of obj) 
         (or position 
             (let ((pos (position-of obj)))
               (setf (position-of obj) (1+ pos))
               pos))))

(defmethod put-byte ((obj byte-buffer) value &key position)
  (setf (svref (source-of obj) 
               (or position
                   (let ((pos (position-of obj)))
                     (setf (position-of obj) (1+ pos))
                     pos)))
        value)
  obj)

(let ((buffer (make-instance 'byte-buffer :capacity 8)))
  (dotimes (i 8)
    (put-byte buffer i))
  (flip buffer)
  (dotimes (i 8)
    (princ (get-byte buffer)))
  (put-byte buffer 9 :position 3)
  (get-byte buffer :position 3))


;; -------- BER Buffers --------

(defclass ber-buffer (byte-buffer)
  ())

(defmethod put-integer ((buffer ber-buffer) number 
                        &key position (tag 2))

       
  (let ((pos (or position (position-of buffer))))
    
  (print tag)
  (print 1)
  (call-next-method buffer n :position position))

(let ((b (make-instance 'byte-buffer)))
  (put-integer b 99))

(let ((c (make-instance 'ber-buffer)))
  (put-integer c 1)
  (put-integer c 2 :tag 77))

(defun put-bind-request (obj)
  
  (put-tag buffer (application 0))
  (let ((content (append (put-integer content (slot-value obj 'version))
    (put-string  content (slot-value obj 'name))
    (put-string  content (slot-value obj 'password) :tag (context 0))
    (flip content)
    (put-length buffer (limit content))
    (put buffer content))
  buffer)

(defun put-bind-request (buffer obj)
  (put-tag buffer (application 0))
  (let ((content ()))
    (put-integer content
  (append (enc-tag (application 0))
          (append (
(defun get-bind-response (ber)
  (assert (eq (get-tag ber) (application 1)))
  (assert (<= (get-length ber) (remaining ber)))
  (let ((obj (make-instance 'bind-response)))
    (setf (slot-value obj 'error-code) (get-integer ber)
          (slot-value obj 'matched-dn) (get-string ber)
          (slot-value obj 'error-msg)  (get-string ber))
    (loop
     (case (peek-tag ber)
           ((referral) (skip ber))
           ((creds)    (setf (slot-value obj 'creds)
                           (get-string ber)))
           (t (return))))
    obj))


