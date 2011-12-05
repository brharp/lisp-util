

;; 2.4.   Entity

(defun mime-entity (content &key content-type encoding)
  (list* `("Content-Type" ,content-type)
         `("Content-Encoding" ,content-encoding)
         (encode content)))


(defstruct message fields body)

(defclass mime-message (message)
  (type)
  (encoding))

(defun multipart/alternative (encoding fields &rest body-parts)
  (let ((b (gen-boundary)))
    (mime-entity :type (format nil "multipart/alternative;boundary=~A" b)
                 :encoding encoding
                 :fields fields
                 :body (encode (serialize body-parts) encoding))))


(mail (organizer cal)
      (multipart/alternative
       (mime-entity (calendar-text cal) :type "text/plain")
       (mime-entity (calendar-html cal) :type "text/html")
       (mime-entity (calendar-ical cal) :type "text/calendar")))

       
                    
 
(multipart-mime-entity 
 (mime-entity)
 (mime-entity))
(defmacro mail ((&rest hdrs &key to from content-type &allow-other-keys)
                &body body)
  (cond
   ((consp body)
    `(make-multipart-mime-entity
      headers (
  `(make-mime-entity headers body))

     
     
                
  (cond ((consp body) (mail-parts body))))

(defmacro mail-part (part)
  (mail ,(first part) ,@
(cond ((consp body) 
       (add-part (mime-part (first body))
                 (multipart-mail (rest body)))))
  
  
(mail (:to "foo@example.com")
      "this is a test")


(let ((boundary (gen-boundary)))
  (mail ("To" (organizer cal)
         :from (me)
         :content-type (multipart/alternative :boundary boundary))
        (((:content-type (text/plain))
          (calendar-text/plain cal))
         ((:content-type (text/html))
          (calendar-text/html cal))
         ((:content-type (text/calendar))
          (calendar-text/calendar cal)))))


(defun mail-parts (part &rest parts)
  (add-part (make-part (part-headers part) (part-content part))
            (mail-parts parts)))

(fold mime-part
      (list text/plain
            text/html
            text/calendar)
      calendar)
      
            

;; 2.10.  Lines

;;    Lines are strings, in which case the content of the string is
;;    the content of the line, to be terminated by CRLF; or, lines are
;;    lists, in which case the line is a header line. The first
;;    element of a header line is the header name, the second element
;;    is the header value.



(defun mime-message (headers content)
  (cond
   ((null content)
    (boundary-delimiter t))
   ((multipartp content)
    (add-part (mime-part (first content))
              (mime-mail (rest content))))))

(defun mime-part (part)
  (boundary-delimiter))


(defun mime-part (headers content)
  (append headers default-mime-part-headers)
  
  
   
