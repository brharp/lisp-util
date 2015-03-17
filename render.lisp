(in-package :lisp-util)

(defun render (it)
  (cond ((atom it) it)
        ((functionp (car it)) (apply (car it) (mapcar #'render (cdr it))))))


(defun render-node (&key title content &allow-other-keys)
  (format nil "<h1>~a</h1><p>~a</p>" title content))


(defun render-page (&key title content &allow-other-keys)
  (format nil "<title>~a</title><body>~a</body>" title content))

(render (list #'render-page :title "Hello, World!"
              :content (list #'render-node :title "Hello, World!"
                             :content "The rain in Spain falls mainly on the plain.")))


