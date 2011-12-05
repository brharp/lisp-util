
;; Function curry.

(defun args (f &rest x)
  (lambda (&rest y)
    (apply f (append x y))))


;; String Extensions

(defun starts-with (prefix string)
  (let ((n (search prefix string)))
    (and n (zerop n))))


;; Synchronization Functions

(defvar *symbol-mutex-monitor* 
  (or #+sbcl (sb-thread:make-mutex))
  "Mutex held by the `synchronized` macro while generating a symbol
mutex.")

(defmacro synchronized ((symbol) &body body)
  (or #+sbcl
      `(sb-thread:with-mutex
        ((get ',symbol 'mutex
           (sb-thread:with-mutex (*symbol-mutex-monitor*)
             (get ',symbol 'mutex
                  (setf (get ',symbol 'mutex)
                        (sb-thread:make-mutex :name (symbol-name ',symbol))
                        )))))
        (progn ,@body))))



;; Iteration Functions

(defmacro while (test &body body)
  `(do () ((not ,test)) (progn ,@body)))


;; Functions for manipulating ranges. A range is a sequence of ordered
;; intervals. Each interval is a pair of values (start, end). The
;; range functions guarantee that intervals are always in order, and
;; there are no overlaps.

;; Ranges consist of intervals.

(defstruct (interval (:print-object print-interval))
  start end)

;; A range is a list of intervals.

(defstruct (range (:print-object print-range))
  first rest)

;; Construct a simple range.

(defun simple-range (start end)
  (cond ((= start end) ())
        ((> start end) (simple-range end start))
        (t (make-range :first (make-interval :start start
                                             :end end)))))

;; Add two non-null ranges.

(defun range+not-null (range1 range2)
  (let* ((intrv1 (range-first range1))
         (intrv2 (range-first range2))
         (start1 (interval-start intrv1))
         (start2 (interval-start intrv2))
         (end1   (interval-end intrv1))
         (end2   (interval-end intrv2)))
    (cond
     ((and (<= start2 end1) (<= start1 end2)) ;intervals overlap
      (make-range :first (make-interval :start (min start1 start2)
                                        :end (max end1 end2))
                  :rest (range+ (range-rest range1)
                                (range-rest range2))))
     ((< start1 start2)
      (make-range :first intrv1
                  :rest (range+ (range-rest range1) range2)))
     (t
      (make-range :first intrv2
                  :rest (range+ range1 (range-rest range2)))))))

;; Allow adding a range to NIL.

(defun range+ (range1 range2)
  (cond
   ((null range1) range2)
   ((null range2) range1)
   (t (range+not-null range1 range2))))

;; Intersection.

(defun range^not-null (range1 range2)
  (let* ((intrv1 (range-first range1))
         (intrv2 (range-first range2))
         (start1 (interval-start intrv1))
         (start2 (interval-start intrv2))
         (end1   (interval-end intrv1))
         (end2   (interval-end intrv2)))
    (cond
     ((and (<= start2 end1) (<= start1 end2)) ;intervals overlap
      (let* ((end (min end1 end2)))
        (make-range :first (make-interval :start (max start1 start2)
                                          :end end)
                    :rest (range^ (range+ (simple-range end end1)
                                          (range-rest range1))
                                  (range+ (simple-range end end2)
                                          (range-rest range2))))))
     ((< start1 start2)
      (range^ (range-rest range1) range2))
     (t
      (range^ range1 (range-rest range2))))))

;; Allow intersecting a range with NIL.

(defun range^ (range1 range2)
  (cond
   ((null range1) nil)
   ((null range2) nil)
   (t (range^not-null range1 range2))))

;; Equality.

(defun interval= (inv1 inv2)
  (and (= (interval-start inv1) (interval-start inv2))
       (= (interval-end inv1) (interval-end inv2))))

(defun range= (range1 range2)
  (or (and (null range1) (null range2))
      (and (interval= (range-first range1) (range-first range2))
           (range= (range-rest range1) (range-rest range2)))))

;; Construct a complex range.

(defun range (&rest args)
  (if (null args) nil
    (range+ (simple-range (car args) (cadr args))
            (apply #'range (cddr args)))))

;; Iterate over the elements of a range.
;; ie. (do-range (i (range 1 5 10 15 12 20)) (print i))

(defmacro do-range ((var range) &body body)
  (let ((stt (gensym))
        (end (gensym))
        (inv (gensym))
        (rng (gensym)))
    `(prog (,var ,stt ,end ,inv (,rng ,range))
       NEXT
         (setq ,inv (range-first ,rng)
               ,stt (interval-start ,inv)
               ,end (interval-end ,inv)
               ,var ,stt)
       INC
         (progn ,@body)
         (incf ,var)
         (when (<= ,var ,end)
           (go INC))
         (setq ,rng (range-rest ,rng))
         (when ,rng
           (go NEXT)))))
   

;; Format an interval.

(defun print-interval (object stream)
  (format stream "~a-~a"
          (interval-start object)
          (interval-end   object)))

;; Format a range.

(defun print-range (object stream)
  (format stream "#<RANGE ")
  (do ((r object (range-rest r)))
      ((null r))
    (format stream "~a" (range-first r))
    (unless (null (range-rest r))
      (format stream ",")))
  (format stream ">"))

;; Convert a range to a list of intervals.

(defun range-list (range)
  (if (null range) ()
    (list* (interval-start (range-first range))
           (interval-end (range-first range))
           (range-list (range-rest range)))))

;; Make a load form.

(defmethod make-load-form ((object range) &optional environment) 
  (cons 'range (range-list object)))


;; Be quiet.
(defmacro quiet ()
  #+sbcl `(declaim (sb-ext:muffle-conditions sb-ext::style-warning)))


(defun select (item sequence &key from-end (test #'eql) (start 0) end
                    count key)
  (remove item sequence :from-end from-end :test (complement test)
          :start start :end end :count count :key key))
