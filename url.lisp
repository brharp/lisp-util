(defstruct url path)

(defun url (string)
  (make-url :path string))

(defun url-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (list 'url (read stream t nil t)))

(defmethod print-object ((object url) stream)
  (format stream "#u\"~a\"" (url-path object)))

(set-dispatch-macro-character #\# #\u #'url-reader)
