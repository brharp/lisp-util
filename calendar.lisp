;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; iCalendar
;;; Copyright 2010 M. Brent Harp



;(eval-when (:load-toplevel)
;  (with-input-from-string (s "a
; bc")
;    (let ((f (make-instance 'folding-character-input-stream
;                            :stream s)))
;      (assert (eq #\a (read-char f)))
;      (assert (eq #\b (read-char f)))
;      (assert (eq #\c (read-char f))))))


;; --------------------------------------------- calendar model ---

;; \\section{The Calendar Model}

(defun calendar-events (vcalendar)
  (select :vevent (component-components vcalendar)
          :key #'component-name))

(defun event-organizer (vevent)
  (property-value (find-property :organizer vevent)))

(defun event-id (vevent)
  (property-value (find-property :uid vevent)))

(defun event-start (vevent)
  (parse-time "~Y~M~D"
  (property-value (find-property :dtstart vevent))))

(defun event-end (vevent)
  (parse-time "~Y~M~D"
  (property-value (find-property :dtend vevent))))

;; \\section{The Document Model}

;; Components

(defun make-component (&key name properties components)
  (cons (cons name properties) components))

(defun component-name (component)
  (caar component))

(defun component-properties (component)
  (cdar component))

(defsetf component-properties (component) (properties)
  `(rplacd (car ,component) ,properties))

(defun component-components (component)
  (cdr component))

(defsetf component-components (component) (components)
  `(rplacd ,component ,components))

(defun component-p (obj)
  (and (consp obj)
       (consp (car obj))
       (atom (caar obj))))

;; Properties

(defun make-property (&key name parameters value)
  (cons (cons name parameters) value))

(defun property-name (prop)
  (caar prop))

(defun property-parameters (prop)
  (cdar prop))

(defun property-value (prop)
  (cdr prop))

(defun property-p (obj)
  (and (consp obj)
       (consp (car obj))
       (atom (caar obj))))

(defun find-property (name component)
  (find name (component-properties component)
        :key #'property-name))

;; Parameters

(defun make-parameter (&key name values)
  (cons name values))

(defun parameter-name (par)
  (car par))

(defun parameter-values (par)
  (cdr par))

(defun parameter-p (obj)
  (and (consp obj) (atom (car obj))))

(defun find-parameter (name component)
  (find name (property-parameters component)
        :key #'parameter-name))

;; \\section{Folding Character Input Stream}
;; 
;; Long lines in calendar files are folded. The \verb
;; folding-character-input-stream unfolds lines as they are read. It
;; is used by the function read-calendar.
;; 
;; \\begin{verbatim}

(defclass folding-character-input-stream
  (pushback-character-input-stream)
  ())

(defmethod initialize-instance
  ((instance folding-character-input-stream)
   &key stream)
  (call-next-method instance :stream stream :size 2))

(defmethod stream-read-char
  ((stream folding-character-input-stream))
  (let ((c1 (call-next-method)))
    (cond ((eq c1 #\Return)
           (let ((c2 (call-next-method)))
             (cond ((eq c2 #\Newline)
                    (let ((c3 (call-next-method)))
                      (cond ((or (eq c3 #\Space) (eq c3 #\Tab))
                             (call-next-method))
                            (t (unread-char c3 stream)
                               c2))))
                   ((or (eq c2 #\Space) (eq c2 #\Tab))
                    (call-next-method))
                   (t (unread-char c2 stream) c1))))
          ((eq c1 #\Newline)
           (let ((c2 (call-next-method)))
             (cond ((eq c2 #\Return)
                    (let ((c3 (call-next-method)))
                      (cond ((or (eq c3 #\Space) (eq c3 #\Tab))
                             (call-next-method))
                            (t (unread-char c3 stream) c1))))
                   ((or (eq c2 #\Space) (eq c2 #\Tab))
                    (call-next-method))
                   (t (unread-char c2 stream) c1))))
          (t c1))))

;; -------------------------------------- calendar reader ---

(defvar *calendar*)

(defun read-calendar (&optional (stream *standard-input*))
  "Scans iCalendar into tokens."
  (setq stream
        (make-instance 'folding-character-input-stream
                       :stream stream))
  (let ((*readtable* (copy-readtable nil))
        (*package*   (find-package "KEYWORD"))) 
    (set-macro-character 
     #\; ;; Parameter reader.
     (lambda (stream char)
       (declare (ignore char))
       (make-parameter
        :name (read stream)
        :values (do ((vals nil (cons (read stream) vals)))
                    ((member (peek-char nil stream) '(#\: #\;))
                     vals)))))
    (set-macro-character
     #\= ;; Parameter value reader.
     (lambda (stream char)
       (declare (ignore char))
       (case (peek-char nil stream)
         ((#\") (read stream)) ;; Read quoted strings.
         (t     (next-token () '(#\; #\: #\,) stream)))))   
    (set-syntax-from-char #\, #\=)
    (set-macro-character
     #\: ;; Component name, property value marker.
     (lambda (char stream)
       (declare (ignore char stream))
       (error "Attempt to read ':'.")))
    (labels
     ((component (name &optional (eof-error-p t) eof-value)
       (do ((thing (make-component :name name))
            (token (read stream eof-error-p eof-value)
                   (read stream eof-error-p eof-value)))
           ((eq eof-value token) thing)
         (labels
          ((property (name)
             (make-property 
              :name name
              :parameters
              (do ((p nil (cons (read stream) p)))
                  ((not (char= #\; (peek-char nil stream)))
                   p))
              :value
              (and (assert-curr-char '(#\:) stream)
                   (read-line stream)))))
          (case token
            ((:BEGIN)
             (assert-curr-char '(#\:) stream)
             (push (component (read stream))
                   (component-components thing)))
            ((:END) 
             (assert-curr-char '(#\:) stream)
             (assert (equal (read stream) name))
             (return thing))
            (t
             (push (property token)
                   (component-properties thing))))
         ))))
     (setq *calendar*
           (cadr (component :iCalendar nil :eof))))))



(defun read-calendar-from-string (string)
  (with-input-from-string (stream string)
    (read-calendar stream))) 


(defun parse-time (format string)
  (let ((year 0)
        (month 1)
        (date 1)
        (hour 0)
        (minute 0)
        (second 0)
        (i 0) (j 0))
    (labels ((get-number (n)
               (let ((v 0))
                 (dotimes (k n)
                   (cond
                    ((digit-char-p (char string j))
                     (setq v (* v 10))
                     (incf v (- (char-code (char string j))
                                (char-code #\0)))
                     (incf j))
                    (t (error :parse))))
                 v)))
      (loop
       (cond
        ((= i (length format))
         (return
          (encode-universal-time
           second minute hour date month year)))
        ((char= #\~ (char format i))
         (incf i)
         (cond
          ((= i (length string))
           (error :parse))
          (t (case (char format i)
                   (#\Y (setq year (get-number 4)))
                   (#\M (setq month (get-number 2)))
                   (#\D (setq date (get-number 2)))
                   (#\h (setq hour (get-number 2)))
                   (#\m (setq minute (get-number 2)))
                   (#\s (setq second (get-number 2))))
             (incf i))))
        ((char= (char format i)
                (char string j))
         (incf i)
         (incf j))
        (t
         (error :parse)))))))

        
(defun fold (func initval list)
  (reduce func list :initial-value initval))

(defun format-time (format time)
  "Formats a universal time."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time time)
    (do ((i 0 (1+ i)))
        ((= i (length format)))
        (case (char format i)
              ((#\~) (incf i)
               (when (= i (length format))
                 (error "bad format"))
               (case (char format i)
                     ((#\Y) (princ year))
                     ((#\M) (princ month))
                     ((#\D) (princ date))
                     ((#\h) (princ hour))
                     ((#\m) (princ min))
                     ((#\s) (princ sec))))
              (t (princ (char format i)))))))

(defparameter 
  short-date-format-string 
  "~h:~0m")

(defparameter 
  long-date-format-string
  "~M/~D ~h:~0m")

(defparameter
  full-date-format-string
  "~Y/~M/~D ~h:~0m")

(defun time= (t1 t2)
  "Compares each element of decoding T1 and T2.

Returns multiple values: T for each element of the decoded times that
are identical (by EQL), NIL for all others."
  (let ((d1 (multiple-value-list (decode-universal-time t1)))
        (d2 (multiple-value-list (decode-universal-time t2))))
    (cons (= t1 t2) (values-list (mapcar #'eql d1 d2)))))

#+debug
(let ((ds (get-universal-time))
      (de (get-universal-time)))
  (multiple-value-bind (ssec smin shour sdate smonth syear)
      (decode-universal-time ds)
    (multiple-value-bind (esec emin ehour edate emonth eyear)
         (decode-universal-time de)
         (cond ((not same-year) "~Y/~M/~D ~h:~m")
               ((not same-date) "~M/~D ~h:~m")
               (t               "~h:~m")))))
    


(defconstant iso-date-time "~Y~M~DT~h~m~s")

(defun dtstart (component)
  (property-value (assoc :dtstart (component-properites component))))

(defun dtend (component)
  (property-value (assoc :dtend (component-properties component))))

(defun event-instances (vevent)
  "Returns instances of this event as a list of intervals."
  ;; Obviously this also needs to handle recurrance rules, but I am
  ;; just working with single events for now.
  (make-interval
   :start (parse-time iso-date-time (vevent-dtstart vevent))
   :end (parse-time iso-date-time (vevent-dtend vevent))))

(defun event-free-busy (e)
  "Returns a range of busy times."
  (fold (function range+) ()
        (mapcar (function simple-range)
                (event-intervals e))))

(defun calendar-free-busy (c)
  (fold (function range+) ()
        (mapcar (function event-free-busy)
                (calendar-events c))))

(defun calendar-events (vcalendar)
  (select :vevent (component-components vcalendar)
          :key (function component-name)))


#+debug
(with-open-file
 (stream "/export/brharp/tmp/HTMScheduling.ics")
 (read-calendar stream))

;; List the start and end times of all events organized by
;; brharp@uoguelph.ca.
#+debug
(with-open-file
 (stream "/export/brharp/tmp/ccsvacation.ics")
 (mapcar (lambda (vevent)
           (list (event-id vevent)
                 (event-start vevent)
                 (event-end vevent)))
         (select "mailto:brharp@uoguelph.ca"
                 (calendar-events (read-calendar stream))
                 :key #'event-organizer :test #'equal)))

(defun interval-intersection (x y)
  (if (or (and (>= (cdr x) (car y))
               (<= (car x) (cdr y)))
          (and (>= (cdr y) (car x))
               (<= (car y) (cdr x))))
      (cons (max (car x) (car y))
            (min (cdr x) (cdr y)))))
          

(defun interval-duration (x)
  (if (null x) 0 (- (cdr x) (car x))))

;; Compute the total duration of all events organized by brharp on Aug
;; 9th between 8:30 am and 4:45 pm.
#+debug
(with-open-file
 (stream "/export/brharp/tmp/ccsvacation.ics")
 (let ((y (cons (encode-universal-time 00 30 08 09 08 2010)
                (encode-universal-time 00 45 16 09 08 2010))))
   (reduce #'+ (select "mailto:brharp@uoguelph.ca"
                       (calendar-events (read-calendar stream))
                       :key #'event-organizer :test #'equal)
           :initial-value 0
           :key (lambda (vevent)
                  (interval-duration
                   (interval-intersection
                    (cons (event-start vevent) (event-end vevent))
                    y))))))


;; Select events by attributes such as organizer, etc. (text
;; attributes) and you are left with a list of events. Events can
;; occur multiple times (for repeating events) or span multiple days,
;; so for each event generate a list of intervals (or tree of
;; intervals) that represents each instance. 

;; Before we can compute the total duration of all events over
;; multiple days (the calendar summary) we need a way of generating a
;; series of "work day" intervals. The intersection of these intervals
;; with the calendar data are the intervals we are interested for
;; calculating a calendar summary of working hours.

;; Repeating events in iCalendar are represented by recurrance rules,
;; which look something like the following:

;; RRULE:FREQ=DAILY;UNTIL=20101201T045959Z;INTERVAL=1;BYDAY=MO,TU,WE,TH,FR

;; We need a function to compile RRULE descriptions like the above
;; into lambda expressions that, given an event, generate a list of
;; event instances.

;; The first step is to parse the RRULE into its component parts. The
;; syntax of an RRULE value is the same as iCalendar parameters in
;; general, so we factor out the portions of READ-CALENDAR that deal
;; with parameters into a separate function that can be called on
;; RRULE values.

;; The RRULE (when present) actually provides the primary scheduling
;; information, with DTSTART filling in any missing details. The FREQ
;; parameter dictates how the other paramter values are
;; interpreted. If a BYXXX parameter is not given, it takes a default
;; value or the value of DTSTART, depending on the value of FREQ. For
;; example, given:
;; 
;;    FREQ=DAILY
;; 
;; the default value for BYMONTH is 1-12 inclusive (repeat in every
;; month,) and the default value for BYHOUR is taken from
;; DTSTART. When default values are used, the defaults are really
;; circular lists---in the example above, the month will cycle through
;; 1-12 over and over again until the end of the sequence. The FREQ
;; parameter tells us which field to "roll" with each iteration, the
;; INTERVAL tells us how many elements to skip on each roll, and the
;; BYXXX parameters give explicit values to roll through.

(defun rrule (spec)
  (dolist (part (split-string #\; spec))
    (dolist (nvpair (split-string #\= part))
      ...)))

(defun repeat (start end until interval freq)
  (if (<= end until)
      (cons source (repeat (shift source (* interval freq))
                           until interval freq))))

(defun repeat (vevent)
  (let ((start (event-start vevent))
        (durat (event-duration vevent))
        (step  (* interval freq)))
    (generate until (lambda (i) (+ i step)) start
              :value (lambda (i) (cons i (+ i durat)))
              :test #'>)))
          
            
          

#+debug
(read-calendar-from-string "BEGIN:VCALENDAR
PRODID:Zimbra-Calendar-Provider
VERSION:2.0
METHOD:PUBLISH
BEGIN:VEVENT
UID:ea12ccea-ec01-4a76-b200-8bb94b7d1037
RRULE:FREQ=DAILY;UNTIL=20101201T045959Z;INTERVAL=1;BYDAY=MO,TU,WE,TH,FR
SUMMARY:Test
ORGANIZER;CN=Brent Harp:mailto:brharp@uoguelph.ca
DTSTART:20100913T083000
DTEND:20100913T164500
STATUS:CONFIRMED
CLASS:PUBLIC
TRANSP:OPAQUE
DTSTAMP:20100907T195802Z
SEQUENCE:0
BEGIN:VALARM
ACTION:DISPLAY
TRIGGER;RELATED=START:-PT5M
DESCRIPTION:Reminder
END:VALARM
END:VEVENT
END:VCALENDAR")

(defun rrule ()
  )


;; ------------------------------------------- folding and unfolding ---

(defun fold-lines ()
  (let ((c) (i 0))
    (loop (setq c (read-char nil nil))
          (cond ((null c) (return))
                ((char= c #\Newline)
                 (write-char c)
                 (setq i 0))
                ((> i 75)
                 (write-char #\Newline)
                 (write-char #\Space)
                 (write-char c)
                 (setq i 2))
                (t
                 (write-char c)
                 (incf i))))))

(defun unfold-lines ()
  (let ((c) (i 0))
    (loop (setq c (read-char nil nil))
          (cond ((null c) (return))
                ((char= c #\Newline)
                 (write-char c)
                 (setq i 0))
                ((> i 0) (write-char c)
                 (incf i))
                ((char= c #\Space))
                (t (write-char c)
                   (incf i))))))

