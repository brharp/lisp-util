;; File: xml-template.lisp

(require "xml")

(in-package :xml)

(defun xml-template-print (path env)
  (let* ((template (xml-template-load path))
         (document (xml-template-fill template env)))
    (xml-print document)))

(defun xml-template-load (path)
  (with-open-file (*standard-input* path)
    (xml-document)))

(defun xml-template-fill (template env)
  (xml-template-eval-node template env nil))

(defun xml-template-eval-node (node env parent)
  (cond ((xml-element-node-p node)
         (xml-template-eval-element-node node env parent))
        ((xml-attribute-node-p node)
         (xml-template-eval-attribute-node node env parent))
        ((xml-text-node-p node)
         (xml-template-eval-text-node node env parent))))

(defun xml-template-eval-element-node (node env parent)
  (let ((name (xml-node-name node)))
    (funcall (xml-template-tag-function name) node env parent)))
  

(defvar *xml-template-tag-function*
  (make-hash-table :test #'equal))

(defun xml-template-tag-function (name)
  (or (gethash name *xml-template-tag-function*)
      (function xml-template-default-tag-function)))

(defun xml-template-default-tag-function (node env parent)
  (let* ((name       (xml-node-name node))
         (new-node   (xml-create-element-node name nil nil))
         (new-parent (cons new-node parent)))
    (when (and parent (first parent))
      (xml-append-child (first parent) new-node))
    (dolist (a (xml-attribute-nodes node))
      (xml-template-eval-node a env new-parent))
    (dolist (c (xml-child-nodes node))
      (xml-template-eval-node c env new-parent))
    new-node))

(defun xml-template-eval-attribute-node (node env parent)
  (let* ((name      (xml-node-name node))
         (value     (xml-node-value node))
         (new-value (xml-template-expand-string value env)))
    (xml-set-attribute (first parent) name new-value)))


(defun xml-template-eval-text-node (node env parent)
  (let ((new-text (xml-template-expand-string node env)))
    (xml-append-child (first parent) new-text)
    new-text))

(defun xml-template-expand-string (string env)
  "Expands ${parameter} specifications in string."
  (with-input-from-string (*standard-input* string)
    (with-output-to-string (*standard-output*)
      (xml-template-expand env))))

(defun xml-template-expand (env)
  (do ((char (read-char nil nil) (read-char nil nil)))
      ((null char))
      (case char
        ((#\$) (xml-template-expand-param env))
        (t     (write-char char)))))

(defun xml-template-expand-param (env)
  (match-char #\{)
  (let ((name (xml-template-param-name)))
    (princ (or (getf env name) ""))
    (match-char #\})))

(defun xml-template-name-char-p (char)
  (or (char= #\_ char)
      (char= #\- char)
      (alphanumericp char)))

(defvar *xml-template-symbol-package* (find-package "KEYWORD"))

(defun xml-template-param-name ()
  (do ((char (peek-char nil nil nil) (peek-char nil nil nil))
       (buffer (make-array 256 :fill-pointer 0)))
      ((or (null char) (not (xml-template-name-char-p char)))
       (intern (string-upcase (coerce buffer 'string))
               *xml-template-symbol-package*))
      (vector-push (read-char) buffer)))

(defun empty-p (string)
  (or (null string) (zerop (length string))))

(defun xml-template-tag-when (node env parent)
  (let ((test (xml-get-attribute node "test")))
    (when (not (empty-p (xml-template-expand-string test env)))
      (let ((apply-templates
             #'(lambda (child) (xml-template-eval-node child env parent))))
        (mapcan apply-templates (xml-child-nodes node))))))

(setf (gethash "when" *xml-template-tag-function*)
      (function xml-template-tag-when))

(defun xml-template-tag-set-attribute (node env parent)
  (let ((name (xml-get-attribute node "name"))
        (value (xml-get-attribute node "value")))
    (xml-set-attribute (first parent) name value)
    ()))

(setf (gethash "set-attribute" *xml-template-tag-function*)
      (function xml-template-tag-set-attribute))

(defun xml-template-tag-loop (node env parent)
  (let* ((form (xml-get-attribute node "form"))
         (name (intern (string-upcase form) *xml-template-symbol-package*))
         (lst  (getf env name)))
    (when (and lst (listp lst))
      (mapcar #'(lambda (item)
                  (let ((apply-templates
                         #'(lambda (child) (xml-template-eval-node child item parent))))
                    (mapcan apply-templates (xml-child-nodes node))))
              lst))))

(setf (gethash "loop" *xml-template-tag-function*)
      (function xml-template-tag-loop))

(defun xml-template-tag-with (node env parent)
  (let* ((form   (xml-get-attribute node "form"))
	 (name   (intern (string-upcase form) *xml-template-symbol-package*))
	 (object (get env name)))
    (when (not (null object))
      (let ((apply-templates
	     #'(lambda (child) (xml-template-eval-node child object parent))))
	(mapcan apply-templates (xml-child-nodes node))))))

(setf (gethash "with" *xml-template-tag-function*)
      (function xml-template-tag-with))
