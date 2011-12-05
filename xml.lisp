;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; XML Processing
;;; Copyright (c) 2010 M. Brent Harp

(defun match-char (char)
  (let ((inch (read-char)))
    (unless (char= char inch)
      (error "char mismatch: expected ~s but got ~s"
             char inch))))

(defun xml-document ()
  "Parses an XML document."
  (match-char #\<)
  (xml-element))

(defun xml-element ()
  "Matches an XML element."
  (list* (xml-name)
         (xml-attribute-list)
         (ecase (peek-char t)
           (#\> (xml-open-element))
           (#\/ (xml-empty-tag)))))

(defun xml-name-char-p (char)
  (or (char= #\: char)
      (char= #\_ char)
      (alphanumericp char)))

(defun xml-name ()
  (do ((char (read-char) (read-char))
       (buffer (make-array 256 :fill-pointer 0)))
      ((not (xml-name-char-p char))
       (progn (unread-char char)
              (coerce buffer 'string)))
      (vector-push char buffer)))

(defun xml-attribute-list ()
  (if (xml-name-char-p (peek-char t))
      (cons (xml-attribute)
            (xml-attribute-list))))

(defun xml-attribute ()
  (cons (xml-attribute-name)
        (xml-attribute-value)))

(defun xml-attribute-name ()
  (xml-name))

(defun xml-attribute-value ()
  (match-char #\=)
  (case (peek-char t)
    (#\' (xml-string #\'))
    (#\" (xml-string #\"))
    (t   (xml-name))))

(defun xml-open-element ()
  (match-char #\>)
  (xml-node-list))

(defun xml-child-element ()
  (xml-element))

(defun xml-close-tag ()
  (match-char #\/)
  (xml-name)
  (match-char #\>)
  ())

(defun xml-empty-tag ()
  (match-char #\/)
  (match-char #\>)
  ())

(defun xml-node-list ()
  (case (peek-char t)
    (#\< (read-char)
         (if (eq #\/ (peek-char))
             (xml-close-tag)
             (cons (xml-child-element)
                   (xml-node-list))))
    (t   (cons (xml-text-node)
               (xml-node-list)))))

(defun xml-text-node ()
  (prog1 (read-delimited-string #\<)
    (unread-char #\<)))

(defun xml-string (delim)
  (match-char delim)
  (read-delimited-string delim))

(defun read-delimited-string (delim)
  (do ((char (read-char) (read-char))
       (buffer (make-array 1024 :fill-pointer 0)))
      ((eq delim char)
       (coerce buffer 'string))
      (vector-push char buffer)))

#+debug
(with-input-from-string
 (*standard-input* 
  "<foo x=y><bar>plus<baz a='b'/>text</bar><zap/></foo>")
 (xml-document))

#+debug
(with-input-from-string
 (*standard-input* "foo=bar>")
 (xml-attribute-list))

#+debug
(with-input-from-string
 (*standard-input* "<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\"><soap:Header><context xmlns=\"urn:zimbra\"><authToken>jeiofjeosfjsejfoe832892</authToken><userAgent name=\"brharp@uoguelph.ca\" version=\"1.0\"/></context></soap:Header><soap:Body><GetMiniCalRequest xmlns=\"urn:zimbraMail\"><s>1319947200000</s><e>1323579600000</e><tz id=\"America/New_York\"/><folder><id>10</id></folder></GetMiniCalRequest></soap:Body></soap:Envelope>")
  (xml-document))



;; DOM Functions

(defconstant xml-element-node   1)
(defconstant xml-attribute-node 2)
(defconstant xml-text-node      3)

(defun xml-node-type (node)
  (cond ((atom node) xml-text-node)
        ((atom (cdr node)) xml-attribute-node)
        ((atom (car node)) xml-element-node)))

(defun xml-element-node-p (node)
  (eq (xml-node-type node) xml-element-node))

(defun xml-attribute-node-p (node)
  (eq (xml-node-type node) xml-attribute-node))

(defun xml-text-node-p (node)
  (eq (xml-node-type node) xml-text-node))

(defun xml-node-value (node)
  (cond ((xml-element-node-p node) (xml-node-name node))
        ((xml-attribute-node-p node) (cdr node))
        ((xml-text-node-p node) node)))

(defun xml-child-nodes (node)
  (and (consp node)
       (consp (cdr node))
       (cddr node)))

(defun xml-attribute-nodes (node)
  (and (consp node)
       (consp (cdr node))
       (cadr node)))

(defun xml-node-name (node)
  (cond ((xml-element-node-p node) (car node))
        ((xml-attribute-node-p node) (car node))
        ((xml-text-node-p node) "#TEXT")))

(defun xml-get-attribute (node name)
  (and (xml-element-node-p node)
       (cdr (assoc name (second node) :test #'string=))))

(defun xml-set-attribute (node name value)
  (and (xml-element-node-p node)
       (setf (second node)
             (acons name value (second node)))))

(defun xml-first-child (node)
  (first (xml-child-nodes node)))

(defun xml-decendants (node)
  (if (xml-element-node-p node)
      (cons node (mapcan #'xml-decendants (xml-child-nodes node)))))

(defun xml-get-elements-by-tag-name (node name)
  (remove name (xml-decendants node)
          :key #'xml-node-name :test (complement #'string=)))


;; XML Printer

(defun xml-print (node)
  (cond
    ((xml-element-node-p node) (xml-print-element node))
    ((xml-attribute-node-p node) (xml-print-attribute node))
    ((xml-text-node-p node) (xml-print-text node))))

(defun xml-print-attribute (a)
  (format t " ~a=\"~a\"" (xml-node-name a) (xml-node-value a)))

(defun xml-print-element (node)
  (format t "<~a" (xml-node-name node))
  (if (xml-attribute-nodes node)
      (dolist (a (xml-attribute-nodes node))
        (xml-print-attribute a)))
  (if (xml-child-nodes node)
      (progn (format t ">")
             (dolist (c (xml-child-nodes node))
               (xml-print c))
             (format t "</~a>" (xml-node-name node)))
      (format t "/>")))

(defun xml-print-text (node)
  (princ node))

#+debug
(with-input-from-string
 (*standard-input* "<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\"><soap:Header><context xmlns=\"urn:zimbra\"><authToken>jeiofjeosfjsejfoe832892</authToken><userAgent name=\"brharp@uoguelph.ca\" version=\"1.0\"/></context></soap:Header><soap:Body><GetMiniCalRequest xmlns=\"urn:zimbraMail\"><s>1319947200000</s><e>1323579600000</e><tz id=\"America/New_York\"/><folder><id>10</id></folder></GetMiniCalRequest></soap:Body></soap:Envelope>")
    (xml-print (xml-document)))

(provide "xml")
