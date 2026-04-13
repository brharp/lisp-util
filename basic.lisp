
(defvar *rv* nil "the value environment")
(defvar *rg* nil "the GOTO environment")
(defvar *rn* nil "the NEXT environment")

(defclass value ()
  ())

(defclass environment ()
  ())

(defclass null-variable-env (environment)
  ())

(defclass null-goto-env (environment)
  ())

(defclass full-env (environment)
  ((link :initarg :link :accessor full-env-link)
   (name :initarg :name :accessor full-env-name)))

(defclass variable-env (full-env)
  ((value :initarg :value :accessor variable-env-value)))

(defclass global-env (full-env)
  ())


;; Statements

(defclass statement ()
  ((number :initarg :number :accessor statement-number)
   (keyword :initarg :keyword :accessor statement-keyword)
   (link :initarg :link :accessor statement-link)))

(defclass let-statement (statement)
  ((name :initarg :name :accessor let-statement-name)
   (value :initarg :value :accessor let-statement-value)))


;; Continuations

(defclass continuation ()
  ((k :initarg :k :accessor continuation-k)))

(defclass let-cont (continuation)
  ((n :initarg :n :accessor let-cont-n)
   (r :initarg :r :accessor let-cont-r)))

(defclass program-cont (continuation)
  ((s :initarg :s :accessor program-cont-s)
   (r :initarg :r :accessor program-cont-r)))
  
(defclass bottom-cont (continuation)
  ((f :initarg :f :accessor bottom-cont-f)))


;; Resume

(defmethod resume ((k continuation) v)
  (error "Unknown continuation ~A" k))

(defmethod resume ((k let-cont) v)
  (update (let-cont-r k) (let-cont-n k) (continuation-k k) v))

(defmethod resume ((k program-cont) v)
  (evaluate-program (rest (program-cont-s k))
		    (program-cont-r k)
		    (continuation-k k)))

(defmethod resume ((k goto-cont) v)
  (evaluate-program v (continuation-k k)))

(defmethod resume ((k bottom-cont) v)
  (funcall (bottom-cont-f k) v))


;; Lookups

(defmethod lookup ((r environment) n k)
  (error "Not an environment ~a" r))

(defmethod lookup ((r null-env) n k)
  (error "Unknown variable"))

(defmethod lookup ((r null-goto-env) n k)
  (error "Unknown line number"))

(defmethod lookup ((r full-env) n k)
  (lookup (full-env-link r) n k))

(defmethod lookup ((r variable-env) n k)
  (if (eql n (full-env-name r))
      (resume k (variable-env-value r))
    (lookup (full-env-link r) n k)))

(defmethod lookup ((r global-env) n k)
  (let ((binding (assoc n *rv*)))
    (if binding (resume k (cdr binding))
      (lookup (full-env-link r) n k))))

;; Update

(defmethod update ((r null-env) n k v)
  (error "Unknown variable ~a" n))

(defmethod update ((r full-env) n k v)
  (update (full-env-link r) n k v))

(defmethod update ((r variable-env) n k v)
  (if (eql n (variable-env-name r))
      (progn (setf (variable-env-value r) v)
	     (resume k v))
    (update (full-env-link r) n k v)))

(defmethod update ((r global-env) n k v)
  (progn (setf *rv* (acons n v *rv*))
	 (resume k v)))


;; Evaluate

(defun evaluate-quote (v r k)
  (resume k v))

(defun evaluate-variable (n r k)
  (lookup r n k))

(defun evaluate-let (n e r k)
  (evaluate e r (make-instance 'let-cont :k k :n n :r r)))

(defun evaluate-program (s k)
  (if (consp s)
      (if (consp (cdr s))
	  (evaluate (car s) (make-instance 'program-cont :k k :s s))
	  (evaluate (car s) k))
      ;; what to do here? if we have reached the end of the program there
      ;; is nothing more to do except either quit or return to the
      ;; READY? prompt. This might happen naturally if the initial call
      ;; to evaluate passes a ready-cont 
      (resume k)))

(defun evaluate-goto (n k)
  (lookup *rg* n (make-instance 'goto-cont :k k)))

(defun evaluate (e r k)
  (if (atom e)
      (cond ((symbolp e) (evaluate-variable e r k))
	    (t (evaluate-quote e r k)))
    (case (car e)
	  ((quote) (evaluate-quote (cadr e) r k))
	  ((let)   (evaluate-let (cadr e) (caddr e) r k)))))

(evaluate 'x
	  (make-instance 'variable-env :name 'x :value 42
			 :link (make-instance 'global-env
					      :link (make-instance 'null-env)))
	  (make-instance 'bottom-cont :f #'print))


(evaluate '(let x 42)
	  (make-instance 'global-env
			 :link (make-instance 'null-env))
	  (make-instance 'bottom-cont :f #'print))


;; Alternatives

(defclass if-cont (continuation)
  ((n :initarg :n :accessor if-cont-n)))

(defun translate-if (ec eg)
  (let ((tc (translate ec))
	(tg (translate eg)))
    (lambda (k)
      (funcall tc (lambda (v)
		    (funcall (if v tg k)))))))

(defun if-true-test ()
  (let ((sc '((10 SET X 1)))
	(ec '(= 1 1))
	(et '(GOTO 10))
	(k (make-instance 'bottom-cont)))
    (store *rg* (car sc) sc)
    (store *rv* 'x 0)
    (evaluate-if ec et k)
    (assert (= (fetch *rv* 'x) 0))))

(defun if-false-test ()
  (let ((x 0))
    (basic-prog
     (x)
     10 IF 1 = 1 GOTO 30
     20 SET X = 1
     30 QUIT)
    (assert (= x 1))))

(defun evaluate-if (ec ng k)
  (evaluate ec *rv*
	    (lambda (vc)
	      (evaluate ng *rg*
			(lambda (vg)
			  (resume (if vc vg k))))))) 

(defmethod resume ((k if-cont) v)
  (if v
      (evaluate-goto (if-cont-n k) (continuation-k k))
      (resume (continuation-k k))))

(defun evaluate-if-test ()
  (let ((skip ((20 NOOP))))
    (let ((*rg* (list skip)))
      (evaluate-if (= 1 1) 30 k
		   (make-instance 'bottom-cont
				  :f (lambda ()
  )
	  
