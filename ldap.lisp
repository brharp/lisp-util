

(use "ldif")

(defvar *ldap-host* "auction.ccs.uoguelph.ca")
(defvar *ldap-port* 7389)
(defvar *ldap-stream*)
(defvar *ldap-connection* nil)

(defun ldap-search-s (base scope filter attributes
                           attributes-only-p)
  (sb-ext:run-program 
   *ldapsearch-program* `("-h" ,*ldap-host*
                          "-p" ,(string *ldap-port*)
                          "-b" ,*ldap-base*
                          ,@(if attributes-only-p (list "-A"))
                          "-LLL"
                          filter attributes)))

;; macro ldap-do-search
;;
;; Syntax:
;;  (ldap-do-search (base scope filter attributes
;;                   attributes-only-p) &body body)
;;
;; attributes---a list of symbols
;;
;; Discussion:
;;   Searches directory DIR for entries matching FILTER.
;;  Symbols in the list ATTRIBUTES are bound for each
;;  matching entry and BODY is evaluated.
;;
;;   Ldap attributes may have multiple values. Ldap-do-search
;;  binds only the first of multiple values. To bind attributes
;;  with multiple values, use ldap-do-entries and 
;;  ldap-attribute-values-list.
;;
(defmacro ldap-do-search ((base scope filter attributes-only-p)
                          attributes &body body)
  `(let ((results (ldap-search-s base scope filter attributes 
                                 attributes-only-p)))
    (ldap-do-entries (entry results)
      (apply (lambda ,attributes ,@body)
             (mapcar (lambda (a) (ldap-value a entry))
                     ,attributes)

(defmacro ldap-do-entries (entry results &body body)
  `(do ((,entry (ldap-first-entry ,results) (ldap-next-entry ,entry)))
       ((null ,entry))
     ,@body))





   

