;; ps - postscript interface

(defvar *ps-output* t)

;; Paths

(defun newpath ()
  (format *ps-output* "newpath~%"))

(defun moveto (x y)
  (format *ps-output* "~a ~a moveto~%" x y))

(defun lineto (x y)
  (format *ps-output* "~a ~a lineto~%" x y))

(defun closepath ()
  (format *ps-output* "closepath~%"))

(defun stroke ()
  (format *ps-output* "stroke~%"))

(defun fill ()
  (format *ps-output* "fill~%"))

;; Graphics

(defun gsave ()
  "Saves graphics state."
  (format *ps-ouptput* "gsave~%"))

(defun grestore ()
  "Restores graphics state."
  (format *ps-ouptput* "grestore~%"))

(defun setlinewidth (num)
  (format *ps-output* "~a setlinewidth~%" num))

(defun setgray (num)
  "Sets gray level for fill."
  (format *ps-output* "~a setgray~%" num))

(ps-prog (newpath)
	 (moveto 100 200)
	 (lineto 200 250)
	 (lineto 100 300)
	 (closepath)
	 (gsave)
	 
    

(newpath)
(moveto 100 200)
(lineto 200 250)
(lineto 100 300)
(setlinewidth 2)
(stroke)
(force-output *ps-output*)

(defclass box ()
  (boxes :initarg boxes :reader box-boxes))

(defgeneric paint (box))

(defclass vrule (box)
  (height :initarg :height :reader vrule-height))


