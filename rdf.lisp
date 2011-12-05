
;; rdf.lisp

;; An RDF Database

;; In which we define functions for parsing and querying RDF.

;; \begin{verbatim}



(defvar *rdf-db* ())
(defvar *rdf-string-table* (make-hash-table :test #'equalp))

(defun rdf-intern (string)
  (or (gethash string *rdf-string-table*)
      (setf (gethash string *rdf-string-table*) string)))

(defun rdf-insert (subject predicate object &optional graph)
  (push (mapcar #'rdf-intern (list subject predicate object graph)) *rdf-db*))

(let ((id 0))
  (defun rdf-anonymous-node ()
    (format nil "_:a~4,'0d" (incf id))))

(defun rdf-xml-parse (doc graph)
  (dolist (c (xml-child-nodes doc))
    (rdf-xml-parse-node c graph)))

(defun rdf-xml-parse-node (node graph)
  (cond ((xml-text-node-p node)
         (xml-node-value node))
        ((xml-element-node-p node)
         (let ((about (xml-get-attribute "rdf:about" node))
               (type  (or (xml-get-attribute "rdf:type" node)
                          (xml-node-name node))))
           (rdf-insert about "rdf:type" type graph)
           (dolist (c (xml-child-nodes node))
             (rdf-xml-parse-property c about graph))
           about))))

(defun whitespace-p (char)
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

(defun rdf-xml-parse-property (node subject graph)
  (if (xml-element-node-p node)
      (let ((p (xml-node-name node)))
        (if (equal "Resource" (xml-get-attribute "rdf:parseType" node))
            (let ((anode (rdf-anonymous-node)))
              (rdf-insert subject p anode graph)
              (dolist (c (xml-child-nodes node))
                (rdf-xml-parse-property c anode graph)))
          ;; else
          (dolist (c (xml-child-nodes node))
            (let ((o (rdf-xml-parse-node c graph)))
              (if (or (not (stringp o))
                      (find-if-not 'whitespace-p o)) ;Skip whitespace
                  (rdf-insert subject p o graph))))))))

#+nil
(defvar *countries*)
#+nil
(let ((file "/home/brharp/src/countries"))
  (with-open-file (*standard-input* file)
    (setf *countries* (xml-element))
    (rdf-xml-parse *countries* file)))

(defun rdf-match (pattern bindings db)
  (mapcan (lambda (input)
            (let ((result (pat-match pattern input bindings)))
              (if (not (eq result fail))
                  (list result))))
          db))

(defun rdf-map (function pattern &optional (bindings no-bindings) (db *rdf-db*))
  (mapcan function (rdf-match pattern bindings db)))

(defun rdf-select (where &optional (bindings no-bindings))
  (if (null where) (list bindings)
    (rdf-map (lambda (x) (rdf-select (rest where) x))
             (first where) bindings)))

#+nil
(let ((name (rdf-intern "factbook:name"))
      (tld  (rdf-intern "factbook:internetcountrycode"))
      (etcy (rdf-intern "factbook:electricity_imports"))
      (zero (rdf-intern "0")))
  (rdf-select `((?country ,name ?name ?graph)
                (?country ,tld  ?tld  ?graph)
                (?country ,etcy ,zero ?graph))))

(defun subject (x)
  (first x))

(defun predicate (x)
  (second x))

(defun object (x)
  (third x))

(defun graph (x)
  (fourth x))

;; \\end{verbatim}

;; \\section{The index.}

;; The idea of the index is to store lists of triples under a symbol
;; that contain that symbol.  For triples, you end up with three
;; indexes: one mapping subjects to triples, one mapping predicates to
;; triples, and one matching objects to triples.  The retrieval
;; function should return the shortest matching list.

(defstruct itree
  (first nil) (rest nil) (strings (make-hash-table)))

(defun index (key value itree)
  (cond ((consp value)
         (progn (index (first value) (itree-first itree))
                (index (rest value) (itree-rest itree))))
        ((stringp value)
         (nlist-push value (lookup-string key itree)))))

(defun lookup-string (key itree)
  (or (gethash key (itree-strings itree))
      (setf (gethash key (itree-strings itree))
            (make-empty-nlist))))

