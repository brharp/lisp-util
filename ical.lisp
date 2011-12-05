;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; iCalendar
;;; Copyright (c) 2011 M. Brent Harp

(require "xml")

(defun ical-object ()
  "Parses an iCalendar object."
  (if (string= (ical-name) "BEGIN")
      (ical-component)))

(defun ical-component ()
  (match-char #\:)
  (let* ((name (ical-component-name))
         (object (list* name () (ical-property-list))))
    (match-char #\:)
    (if (string= (ical-component-name) name)
        object)))

(defun ical-name ()
  (do ((char (read-char) (read-char))
       (buffer (make-array 256 :fill-pointer 0)))
      ((not (ical-name-char-p char))
       (progn (unread-char char)
              (coerce buffer 'string)))
      (cond
       ((char= #\Newline char) (match-char #\Space))
       ((graphic-char-p char) (vector-push (char-upcase char) buffer)))))

(defun ical-component-name ()
  (do ((char (read-char) (read-char))
       (buffer (make-array 256 :fill-pointer 0)))
      ((not (or (char= #\Return char) (ical-name-char-p char)))
       (coerce buffer 'string))
      (cond
       ((char= #\Return char)) ;skip
       ((graphic-char-p char)
        (vector-push (char-upcase char) buffer)))))

(defun ical-name-char-p (char)
  (or (char= #\- char)
      (alphanumericp char)))

(defun ical-property-list ()
  (let ((name (ical-name)))
    (cond ((string= name "BEGIN")
           (cons (ical-component) (ical-property-list)))
          ((not (string= name "END"))
           (cons (list name
                       (ical-parameter-list)
                       (ical-value))
                 (ical-property-list))))))


(defun ical-parameter-list ()
  (if (char= #\; (peek-char t))
      (cons (ical-parameter)
            (ical-parameter-list))))

(defun ical-parameter ()
  (match-char #\;)
  (cons (ical-parameter-name)
        (ical-parameter-value)))

(defun ical-parameter-name ()
  (ical-name))

(defun ical-parameter-value ()
  (match-char #\=)
  (case (peek-char)
    (#\" (ical-quoted-string #\"))
    (t   (ical-param-text))))

(defun ical-param-text ()
  (do ((char (read-char) (read-char))
       (buffer (make-array 512 :fill-pointer 0)))
      ((or (char= #\" char)
           (char= #\; char)
           (char= #\: char)
           (char= #\, char))
       (progn (unread-char char)
              (coerce buffer 'string)))
      (cond
        ((char= #\Newline char) (match-char #\Space))
        ((graphic-char-p char) (vector-push char buffer)))))

(defun ical-quoted-string (delim)
  (match-char delim)
  (ical-read-delimited-string delim))

(defun ical-read-delimited-string (delim)
  (do ((char (read-char) (read-char))
       (buffer (make-array 1024 :fill-pointer 0)))
      ((char= delim char)
       (coerce buffer 'string))
      (cond
        ((char= #\Newline char) (match-char #\Space))
        ((graphic-char-p char) (vector-push char buffer)))))

(defun ical-value ()
  (match-char #\:)
  (do ((char (read-char) (read-char))
       (buffer (make-array 1024 :fill-pointer 0)))
      ((and (char= #\Newline char)
            (not (char= #\Space (peek-char))))
       (coerce buffer 'string))
      (cond
       ((char= #\Newline char) (match-char #\Space))
       ((graphic-char-p char) (vector-push char buffer)))))

  
(defun ical-parse-date (string)
  (encode-universal-time
   (parse-integer string :start 13 :end 15) ;second
   (parse-integer string :start 11 :end 13) ;minute
   (parse-integer string :start  9 :end 11) ;hour
   (parse-integer string :start  6 :end  8) ;date
   (parse-integer string :start  4 :end  6) ;month
   (parse-integer string :start  0 :end  4) ;year
   ));timezone

(defun ical-recur ()
  (let ((char (peek-char nil nil nil)))
    (when (not (null char))
      (when (or (char= #\: char) (char= #\; char))
        (match-char char))
      (cons (cons (ical-name) (ical-recur-value))
            (ical-recur)))))

(defun ical-recur-value ()
  (match-char #\=)
  (do ((char (read-char nil nil) (read-char nil nil))
       (buffer (make-array 512 :fill-pointer 0)))
      ((or (null char)
           (char= #\" char)
           (char= #\; char)
           (char= #\: char))
       (progn (when char (unread-char char))
              (coerce buffer 'string)))
      (vector-push char buffer)))

(defstruct ical-recur
  freq interval by-day)

(defun ical-parse-list (string)
  (let ((comma (position #\, string)))
    (if comma (cons (subseq string 0 comma) (ical-parse-list (subseq string (1+ comma))))
      (list string))))

(defun ical-get-parameter (property name)
  (xml-get-attribute property name))

(defun ical-parse-recur (string)
  (with-input-from-string (*standard-input* string)
   (let ((doc (list "RRULE" (ical-recur))))
     (make-ical-recur
      :freq     (ical-parse-freq (ical-get-parameter doc "FREQ"))
      :interval (ical-parse-interval (ical-get-parameter doc "INTERVAL"))
      :by-day   (ical-parse-by-day (ical-get-parameter doc "BYDAY"))))))
      
(defun ical-parse-freq (doc)
  (intern (string-upcase doc) :keyword))

(defun ical-parse-interval (doc)
  (parse-integer doc))

(defun ical-parse-day (doc)
  (position doc '("MO" "TU" "WE" "TH" "FR" "SA" "SU") :test #'equal))

(defun ical-parse-by-day (doc)
  (mapcar #'ical-parse-day (ical-parse-list doc)))

          
#+debug
(with-input-from-string
 (*standard-input* "BEGIN:VCALENDAR
PRODID:Zimbra-Calendar-Provider
VERSION:2.0
METHOD:PUBLISH
BEGIN:VEVENT
SUMMARY:TEST 1
DTSTART;VALUE=DATE:20111124
DTEND;VALUE=DATE:20111125
END:VEVENT
BEGIN:VEVENT
SUMMARY:TEST 2
DTSTART;VALUE=DATE:20111124
DTEND;VALUE=DATE:20111125
ATTENDEE;PARTSTAT=\"ACCEPTED\":mailto:brharp@uog
 uelph.ca
END:VEVENT
END:VCALENDAR
")
 (ical-object))

#+debug
(with-open-file (*standard-input* "test.ics")
  (ical-object))


(defun ical-days-in-month (month year)
  (let ((leap (and (not (zerop (mod year 400))) (zerop (mod year 4)))))
    (nth (1- month) (list 31 (if leap 29 28) 31 30 31 30 30 31 30 31 30 31))))

(defun ical-add-interval (time freq interval)
  (cond ((string= "YEARLY" (string freq))
         (multiple-value-bind (second minute hour date month year)
            (decode-universal-time time)
          (when (<= date (ical-days-in-month month year))
            (encode-universal-time second minute hour date month (+ year interval)))))
        ((string= "MONTHLY" (string freq))
         (multiple-value-bind (second minute hour date month year)
            (decode-universal-time time)
          (let* ((temp  (+ month interval))
                 (year  (+ year (floor (/ (1- temp) 12))))
                 (month (1+ (mod (1- temp) 12))))
            (when (<= date (ical-days-in-month month year))
              (encode-universal-time second minute hour date month year)))))
        ((string= "WEEKLY" (string freq))
         (+ time (* interval 60 60 24 7)))           
        ((string= "DAILY" (string freq))
         (+ time (* interval 60 60 24)))))

#+debug
(let ((time (encode-universal-time 0 0 8 31 12 2011)))
  (decode-universal-time (ical-add-interval time :monthly 0)))

#+debug
(assert (equal '(3534325200 3542184000 3565947600 3573720000 3597483600)
               (let ((time (encode-universal-time 0 0 8 31 12 2011)))
                 (ical-repeat time :monthly 3 :count 5))))

(defun ical-apply-rrule (start recur)
  (ical-repeat start (ical-recur-freq recur) (ical-recur-interval recur)
               :until (ical-recur-until recur) :count (ical-recur-count recur)
               :by-years (ical-recur-by-year recur) :by-months (ical-recur-by-months recur)
               :by-days (ical-recur-by-day recur) :by-dates (ical-recur-by-date recur)))


(defun ical-repeat (start freq interval &key (until (expt 2 50)) (count 200)
                          by-years by-months by-days by-dates)
  (do ((i 1 (+ i 1))
       (time start (ical-add-interval start freq (* i interval)))
       (recurrences ()))
      ((or (zerop count) (> i 200)) (nreverse recurrences))
      (when (not (null time))
        (ical-repeat-yearly
         (lambda (time)
           (when (and (plusp count) time (>= time start) (< time until))
             (push time recurrences)
             (decf count)))
         time freq by-years by-months by-days by-dates))))

(defun ical-repeat-yearly (fn time freq by-years by-months by-days by-dates)
  (multiple-value-bind (sec min hour date month year)
   (decode-universal-time time)
   (dolist (by-year (or by-years (list year)))
     (when (or (string= "YEARLY" (string freq))
               (= by-year year))
       (ignore-errors
         (let ((time (encode-universal-time sec min hour date month by-year)))
           (ical-repeat-monthly fn time freq by-months by-days by-dates)))))))

(defun ical-repeat-monthly (fn time freq by-months by-days by-dates)
  (multiple-value-bind (sec min hour date month year)
   (decode-universal-time time)
   (dolist (by-month (or by-months (list month)))
     (when (or (string= "YEARLY"  (string freq))
               (string= "MONTHLY" (string freq))
               (= by-month month))
       (ignore-errors
         (let ((time (encode-universal-time sec min hour date by-month year)))
           (ical-repeat-weekly fn time freq by-days by-dates)))))))

(defun ical-repeat-weekly (fn time freq by-days by-dates)
  (multiple-value-bind (sec min hour date month year day)
   (decode-universal-time time)
   (dolist (by-day (or by-days (list day)))
     (when (or (string= "YEARLY"  (string freq))
               (string= "MONTHLY" (string freq))
               (string= "WEEKLY"  (string freq))
               (= by-day day))
       (let ((time (+ time (* 60 60 24 (- by-day day)))))
         (ical-repeat-daily fn time freq by-dates))))))

(defun ical-repeat-daily (fn time freq by-dates)
  (multiple-value-bind (sec min hour date month year)
   (decode-universal-time time)
   (dolist (by-date (or by-dates (list date)))
     (when (or (string= "YEARLY"  (string freq))
               (string= "MONTHLY" (string freq))
               (string= "WEEKLY"  (string freq))
               (string= "DAILY"   (string freq))
               (= by-date date))
       (when (<= by-date (ical-days-in-month month year))
         (funcall fn (encode-universal-time sec min hour by-date month year)))))))

#+debug
(assert (equal (let ((time (encode-universal-time 0 0 8 1 1 2011)))
                 (mapcar #'ical-print-date-to-string
                         (ical-repeat time :monthly 1 :count 10 
                                      :by-dates '(31))))
               '("2011-01-31 08:00:00 MO" "2011-03-31 08:00:00 TH" "2011-05-31 08:00:00 TU" 
                 "2011-08-31 08:00:00 WE" "2011-10-31 08:00:00 MO" "2011-12-31 08:00:00 SA" 
                 "2012-01-31 08:00:00 TU" "2012-03-31 08:00:00 SA" "2012-05-31 08:00:00 TH" 
                 "2012-08-31 08:00:00 FR")))

#+debug
(assert (equal (let ((time (encode-universal-time 0 0 8 1 1 2011)))
                 (mapcar #'ical-print-date-to-string
                         (ical-repeat time :weekly 2 :count 10 :by-days '(1 3))))
               '("2011-01-11 08:00:00 TU" "2011-01-13 08:00:00 TH" "2011-01-25 08:00:00 TU" 
                 "2011-01-27 08:00:00 TH" "2011-02-08 08:00:00 TU" "2011-02-10 08:00:00 TH" 
                 "2011-02-22 08:00:00 TU" "2011-02-24 08:00:00 TH" "2011-03-08 08:00:00 TU"
                 "2011-03-10 08:00:00 TH")))


#+debug
(assert (equal (let ((time (encode-universal-time 0 0 8 1 1 2011)))
                 (mapcar #'ical-print-date-to-string
                         (ical-repeat time :yearly 1 :count 10 
                                      :by-months '(2 4 6 8) :by-dates '(11 21 31))))
               '("2011-02-11 08:00:00 FR" "2011-02-21 08:00:00 MO" "2011-04-11 08:00:00 MO" 
                 "2011-04-21 08:00:00 TH" "2011-06-11 08:00:00 SA" "2011-06-21 08:00:00 TU"
                 "2011-08-11 08:00:00 TH" "2011-08-21 08:00:00 SU" "2011-08-31 08:00:00 WE" 
                 "2012-02-11 08:00:00 SA")))



(defun ical-print-date (time)
  (multiple-value-bind
   (second minute hour date month year day)
   (decode-universal-time time)
   (format t "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D ~A~%"
           year month date hour minute second
           (nth day '(MO TU WE TH FR SA SU)))))

(defun ical-print-date-to-string (time)
  (multiple-value-bind
   (second minute hour date month year day)
   (decode-universal-time time)
   (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D ~A"
           year month date hour minute second
           (nth day '(MO TU WE TH FR SA SU)))))


;;======================================================================
;; Events
;;======================================================================

(defstruct ical-event 
  dtstart dtend rrule summary)

(defun ical-get-property (doc name)
  (first (xml-get-elements-by-tag-name doc name)))

(defun ical-parse-event (doc)
  (make-ical-event 
   :summary (ical-parse-summary (ical-get-property doc "SUMMARY"))
   :dtstart (ical-parse-dtstart (ical-get-property doc "DTSTART"))
   :dtend   (ical-parse-dtend   (ical-get-property doc "DTEND"))
   :rrule   (ical-parse-rrule   (ical-get-property doc "RRULE"))))

(defun ical-parse-summary (doc)
  (xml-first-child doc))

(defun ical-parse-dtstart (doc)
  (ical-parse-date (xml-first-child doc)))

(defun ical-parse-dtend (doc)
  (ical-parse-date (xml-first-child doc)))

(defun ical-parse-rrule (doc)
  (ical-parse-recur (xml-first-child doc)))

;; xpath syntax would make this much nicer, i.e.:
;;   (xpath doc "DTSTART[1]/text()")

(defun ical-get-component (doc name)
  (xml-get-elements-by-tag-name doc name))

(defstruct ical-calendar events)

(defun ical-parse-calendar (object)
  (let ((events (ical-get-component object "VEVENT")))
    (make-ical-calendar
     :events (mapcar #'ical-parse-event events))))

#+debug
(with-open-file (*standard-input* "test.ics")
                (ical-parse-calendar (ical-object)))

(defun ical-repeat-event (e)
  (let ((dtstart (ical-event-dtstart e))
        (rrule   (ical-event-rrule e)))
    (if (null rrule) (list dtstart)
      (ical-repeat dtstart (ical-recur-freq rrule)
                   (ical-recur-interval rrule)
                   :by-days (ical-recur-by-day rrule)))))

(defun ical-duration (event)
  (let ((dtend (ical-event-dtend event)))
    (if dtend (- dtend (ical-event-dtstart event))
      (ical-event-duration event))))

(defun ical-print-calendar (cal)
  (dolist (e (ical-calendar-events cal))
    (dolist (r (ical-repeat-event e))
      (format t "~A (~3D) ~A ~%"
              (ical-print-date-to-string r)
              (floor (/ (ical-duration e) 60))
              (ical-event-summary e)))))

#+debug
(with-open-file
 (*standard-input* "test.ics")
 (ical-print-calendar (ical-parse-calendar (ical-object))))

(provide "ical")
