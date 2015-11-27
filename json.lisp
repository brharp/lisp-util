
(defun json-object ()
  (match-char #\{)
  (json-property-list))
  
(defun json-property-list ()
  (unless (char= (peek-char t) #\})
    (cons (json-property) (json-property-list))))

(defun json-property ()
  (cons (json-property-key)
        (json-property-value)))

(defun json-property-key ()
  (json-string))
  
(defun json-string ()
  (xml-string #\"))

(defun json-property-value ()
  (match-char #\:)
  (json-value))

(defun json-value ()
  (ecase (peek-char t)
    (#\{  (json-object))
    (#\[  (json-array))
    (#\"  (json-string))
    (t    (let ((ch (read-char)))
            (if (or (char= #\- ch) (numericp ch))
               (unread-char ch)
                (json-number))))))
