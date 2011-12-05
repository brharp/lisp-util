(require "posix")
(require "xml")
(require "http")

(defvar *zimbra-header* "Content-Type: application/soap+xml")
(defvar *zimbra-protocol* "https")
(defvar *zimbra-host* "localhost")
(defvar *zimbra-port* 443)
(defvar *zimbra-session* "")
(defvar *zimbra-account* "")
(defvar *zimbra-password* "")
(defvar *zimbra-auth-token* "")
(defvar *zimbra-ua-name* "zmlisp")
(defvar *zimbra-ua-version* "1.0")
(defvar *zimbra-soap-response* nil)
(defvar *zimbra-debug* nil)
(defvar *zimbra-admin-host* "localhost")
(defvar *zimbra-admin-port* 7071)
(defvar *zimbra-admin-account* "")
(defvar *zimbra-admin-password* "")
(defvar *zimbra-admin-auth-token* "")

;;==================================================================
;; SOAP
;;==================================================================

(defun zimbra-soap (url &key (protocol *zimbra-protocol*) (host *zimbra-host*)
                        (port *zimbra-port*) (account *zimbra-account*)
                        (session (zimbra-session))
                        (auth-token *zimbra-auth-token*)
                        (header *zimbra-header*)
                        (body '("NoOpRequest" ()))
                        (debug *zimbra-debug*))
  (let ((*standard-input*
         (http url :protocol protocol :host host :port port :header
               header :debug debug
               :body (with-output-to-string 
                       (*standard-output*)
                       (format t "<soap:Envelope xmlns:soap='http://www.w3.org/2003/05/soap-envelope'>")
                       (format t "<soap:Header>")
                       (format t "<context xmlns='urn:zimbra'>")
                       (format t "<userAgent name=\"~A\" version=\"~A\"/>" *zimbra-ua-name* *zimbra-ua-version*)
                       (when account (format t "<account>~a</account>" account))
                       (when auth-token (format t "<authToken>~a</authToken>" auth-token))
                       (when session (format t "<session id='~a'/>" session))
                       (when session (format t "<sessionId id='~a'/>" session))
                       (format t "</context>")
                       (format t "</soap:Header>")
                       (format t "<soap:Body>")
                       (xml-print body)
                       (format t "</soap:Body>")
                       (format t "</soap:Envelope>")))))
    ;; Parse response
    (setq *zimbra-soap-response* (xml-document))
    (close *standard-input*)
    (when debug (format t "ZIMBRA: DEBUG: SOAP-RESPONSE: ~s~%" *zimbra-soap-response*))
    *zimbra-soap-response*))

(defun zimbra-admin-soap (url &key (host *zimbra-admin-host*) (port *zimbra-admin-port*)
                              (account *zimbra-admin-account*) (auth-token *zimbra-admin-auth-token*)
                              (header *zimbra-header*) (body '("NoOpRequest" ()))
                              (debug *zimbra-debug*))
  (zimbra-soap url :protocol "https" :host host :port port :account account
               :auth-token auth-token :header header :body body :debug debug))

(defun zimbra-session (&optional (doc *zimbra-soap-response*))
  (xml-first-child (first (xml-get-elements-by-tag-name doc "session"))))


;;==============================================================
;; Authentication 
;;==============================================================

(defun zimbra-auth-request (account password)
  (zimbra-soap "/service/soap/AuthRequest"
               :body `("AuthRequest" (("xmlns" . "urn:zimbraAccount"))
                       ("account" (("by" . "name")) ,account)
                       ("password" () ,password))))

(defun zimbra-admin-auth-request (name password)
  (zimbra-admin-soap "/service/admin/soap/AuthRequest"
                     :body `("AuthRequest" (("xmlns" . "urn:zimbraAdmin"))
                             ("account" (("by" . "name")) ,name)
                             ("password" () ,password))))

(defun zimbra-admin-delegate-auth-request (name &key (duration 86400))
  (zimbra-admin-soap "/service/admin/soap/DelegateAuthRequest"
                     :body `("DelegateAuthRequest" (("xmlns" . "urn:zimbraAdmin")
                                                    ("duration" . ,duration))
                             ("account" (("by" . "name")) ,name))))
  
(defun zimbra-auth (&optional (name *zimbra-account*) (password *zimbra-password*))
  (let ((auth-response (zimbra-auth-request name password)))
    (let ((auth-token (first (xml-get-elements-by-tag-name auth-response "authToken"))))
      (setq *zimbra-account* name *zimbra-auth-token* (xml-first-child auth-token)))))

(defun zimbra-admin-auth (&optional (name *zimbra-admin-account*) (password *zimbra-admin-password*))
  (let ((auth-response (zimbra-admin-auth-request name password)))
    (let ((auth-token (first (xml-get-elements-by-tag-name auth-response "authToken"))))
      (setq *zimbra-admin-account* name *zimbra-admin-auth-token* (xml-first-child auth-token)))))

(defun zimbra-admin-delegate-auth (name)
  (let ((response (zimbra-admin-delegate-auth-request name)))
    (let ((auth-token (first (xml-get-elements-by-tag-name response "authToken"))))
      (setq *zimbra-account* name *zimbra-auth-token* (xml-first-child auth-token)))))

;;=================================================================
;; Folders
;;=================================================================

(defstruct zimbra-folder id name view n)

(defun zimbra-parse-folder (element)
  (make-zimbra-folder
   :id   (xml-get-attribute element "id")
   :name (xml-get-attribute element "name")
   :view (xml-get-attribute element "view")
   :n    (xml-get-attribute element "n")))

(defun zimbra-get-folder-request ()
  (zimbra-soap "/service/soap/GetFolderRequest"
               :body `("GetFolderRequest" (("xmlns" . "urn:zimbraMail")))))

(defun zimbra-folder-action-request (id op)
  (zimbra-soap "/service/soap/FolderActionRequest"
               :body `("FolderActionRequest" (("xmlns" . "urn:zimbraMail"))
                       ("action" (("op" . ,op) ("id" . ,id))))))

(defun zimbra-folders ()
  (let ((gf-response (zimbra-get-folder-request)))
    (let ((fldrs (xml-get-elements-by-tag-name gf-response "folder")))
      (mapcar #'zimbra-parse-folder fldrs))))

(defun zimbra-delete-folder (id)
  (zimbra-folder-action-request id "delete"))


;;=====================================================================
;; Search
;;=====================================================================

(defun zimbra-search-request (query &key (limit "2000") (sort-by "none") 
                                    (types "") (cal-expand-inst-start "0")
                                    (cal-expand-inst-end "0"))
  (zimbra-soap "/service/soap/SearchRequest"
               :body `("SearchRequest" (("limit" . ,limit)
                                        ("sortBy" . ,sort-by)
                                        ("types" . ,types)
                                        ("calExpandInstStart" . ,cal-expand-inst-start)
                                        ("calExpandInstEnd" . ,cal-expand-inst-end)
                                        ("xmlns" . "urn:zimbraMail"))
                       ("query" () ,query))))





;;=========================================================================
;; Domains
;;=========================================================================

(defstruct zimbra-domain id name)

(defun zimbra-parse-domain (element)
  (make-zimbra-domain 
   :id   (xml-get-attribute element "id")
   :name (xml-get-attribute element "name")))

(defun zimbra-get-all-domains-request ()
  (zimbra-soap "/service/admin/soap/GetAllDomainsRequest"
               :body '("GetAllDomainsRequest" (("xmlns" . "urn:zimbraAdmin")))))

(defun zimbra-domains ()
  (let ((gad-response (zimbra-get-all-domains-request)))
    (let ((domains (xml-get-elements-by-tag-name gad-response "domain")))
      (mapcar #'zimbra-parse-domain domains))))


;;==========================================================================
;; Distribution Lists
;;==========================================================================

(defstruct zimbra-distribution-list id name members)

(defun zimbra-parse-distribution-list (e)
  (let ((dl (make-zimbra-distribution-list
             :id      (xml-get-attribute e "id")
             :name    (xml-get-attribute e "name")
             :members (list))))
    (dolist (dlm (xml-get-elements-by-tag-name e "dlm"))
      (push (xml-first-child dlm) (zimbra-distribution-list-members dl)))
    dl))

(defun zimbra-get-all-distribution-lists-request (domain &key (by "id"))
  (zimbra-soap "/service/admin/soap/GetAllDistributionListsRequest"
               :body `("GetAllDistributionListsRequest" (("xmlns" . "urn:zimbraAdmin"))
                       ("domain" (("by" . ,by)) ,domain))))

(defun zimbra-distribution-lists (domain)
  (let ((gadl-response (zimbra-get-all-distribution-lists-request domain :by "name")))
    (let ((dls (xml-get-elements-by-tag-name gadl-response "dl")))
      (mapcar #'zimbra-parse-distribution-list dls))))
  
(defun zimbra-get-distribution-list-request (dl &key (by "id"))
  (zimbra-soap "/service/admin/soap/GetDistributionListRequest"
               :body `("GetDistributionListRequest" (("xmlns" . "urn:zimbraAdmin"))
                       ("dl" (("by" . ,by)) ,dl))))

(defun zimbra-distribution-list (name)
  (let ((gdl-response (zimbra-get-distribution-list-request name :by "name")))
    (let ((dl (first (xml-get-elements-by-tag-name gdl-response "dl"))))
      (zimbra-parse-distribution-list dl))))


;;===========================================================================
;; Accounts
;;===========================================================================

(defstruct zimbra-account id name)

(defun zimbra-parse-account (element)
  (make-zimbra-account
   :id   (xml-get-attribute element "id")
   :name (xml-get-attribute element "name")))

(defun zimbra-create-account-request (name password)
  (zimbra-soap "/service/admin/soap/CreateAccountRequest"
               :body `("CreateAccountRequest" (("xmlns" . "urn:zimbraAdmin"))
                       ("name"     () ,name)
                       ("password" () ,password))))

(defun zimbra-create-account (name password)
  (let ((ca-response (zimbra-create-account-request name password)))
    (let ((account (first (xml-get-elements-by-tag-name ca-response "account"))))
      (zimbra-parse-account account))))


(defun zimbra-modify-account-request (id attributes)
  (zimbra-soap "/service/admin/soap/ModifyAccountRequest"
               :body `("ModifyAccountRequest" (("xmlns" . "urn:zimbraAdmin"))
                       ("id" () ,id)
                       ,@(mapcar
                          (lambda (a) `("a" (("n" . ,(car a))) ,(cdr a)))
                          attributes))))

(defun zimbra-get-account-request (account &key (by "name"))
  (zimbra-soap "/service/admin/soap/GetAccountRequest"
               :body `("GetAccountRequest" (("xmlns" . "urn:zimbraAdmin"))
                       ("account" (("by" . ,by)) ,account))))

(defun zimbra-account (name)
  (let ((response (zimbra-get-account-request name :by "name")))
    (let ((account (first (xml-get-elements-by-tag-name response "account"))))
      (zimbra-parse-account account))))

(defun zimbra-set-account-cos (account cos-name)
  (let ((account-id (zimbra-account-id account))
        (cos-id (zimbra-cos-id (zimbra-cos cos-name))))
    (zimbra-modify-account-request account-id `(("zimbraCOSId" . ,cos-id)))))



;;======================================================================
;; Directory
;;======================================================================    

(defun zimbra-admin-search-directory-request
  (query &key limit offset domain apply-cos max-results attrs sort-by 
         sort-ascending types)
  (let ((body `("SearchDirectoryRequest" (("xmlns" . "urn:zimbraAdmin"))
                ("query" () ,query))))
    (when limit (xml-set-attribute body "limit" limit))
    (when offset (xml-set-attribute body "offset" offset))
    (when domain (xml-set-attribute body "domain" domain))
    (when apply-cos (xml-set-attribute body "applyCos" apply-cos))
    (when max-results (xml-set-attribute body "maxResults" max-results))
    (when attrs (xml-set-attribute body "attrs" attrs))
    (when sort-by (xml-set-attribute body "sortBy" sort-by))
    (when sort-ascending (xml-set-attribute body "sortAscending" sort-ascending))
    (when types (xml-set-attribute body "types" types))
    (zimbra-admin-soap "/service/admin/soap/SearchDirectoryRequest"
                       :body body)))

(defun zimbra-admin-search-directory
  (query &key limit offset domain apply-cos max-results attrs sort-by 
         sort-ascending types)
  (let ((response (zimbra-admin-search-directory-request 
                   query :limit limit :offset offset :domain domain 
                   :apply-cos apply-cos :max-results max-results
                   :attrs attrs :sort-by sort-by
                   :sort-ascending sort-ascending :types types)))
    (xml-child-nodes (first (xml-get-elements-by-tag-name response "SearchDirectoryResponse")))))

;;======================================================================
;; Class of Service (COS)
;;======================================================================

(defstruct zimbra-cos id name)

(defun zimbra-parse-cos (element)
  (make-zimbra-cos
   :id   (xml-get-attribute element "id")
   :name (xml-get-attribute element "name")))

(defun zimbra-get-cos-request (cos &key (by "id"))
  (zimbra-soap "/service/admin/soap/GetCosRequest"
               :body `("GetCosRequest" (("xmlns" . "urn:zimbraAdmin"))
                       ("cos" (("by" . ,by)) ,cos))))

(defun zimbra-cos (name)
  (let ((gc-response (zimbra-get-cos-request name :by "name")))
    (let ((cos (first (xml-get-elements-by-tag-name gc-response "cos"))))
      (zimbra-parse-cos cos))))


;;======================================================================
;; Shares
;;======================================================================

(defun zimbra-get-share-info-request ()
  (zimbra-soap "/service/soap/GetShareInfoRequest"
               :body `("GetShareInfoRequest" (("xmlns" . "urn:zimbraAccount"))
                       ("grantee" (("type" . "grp"))))))

(defun zimbra-shares ()
  (let ((response (zimbra-get-share-info-request)))
    (let ((shares (xml-get-elements-by-tag-name response "share")))
      (mapcar #'zimbra-parse-share shares))))

(defstruct zimbra-share 
  owner-id owner-email owner-name
  folder-id folder-path view mid rights
  grantee-type grantee-id grantee-name
  grantee-display-name)

(defun zimbra-parse-share (e)
  (make-zimbra-share :owner-id     (xml-get-attribute e "ownerId")
                     :owner-email  (xml-get-attribute e "ownerEmail")
                     :owner-name   (xml-get-attribute e "ownerName")
                     :folder-id    (xml-get-attribute e "folderId")
                     :folder-path  (xml-get-attribute e "folderPath")
                     :view         (xml-get-attribute e "view")
                     :mid          (xml-get-attribute e "mid")
                     :rights       (xml-get-attribute e "rights")
                     :grantee-type (xml-get-attribute e "granteeType")
                     :grantee-id   (xml-get-attribute e "granteeId")
                     :grantee-name (xml-get-attribute e "granteeName")
                     :grantee-display-name (xml-get-attribute e "granteeDisplayName")))

(defun zimbra-create-mountpoint-request (folder name &key (view "") (color "1")
                                                (flags "") (zid "") (owner "")
                                                (rid "") (path ""))
  (zimbra-soap "/service/soap/CreateMountpointRequest"
               :body `("CreateMountpointRequest" (("xmlns" . "urn:zimbraMail"))
                       ("link" (("l" . ,folder) ("name" . ,name) ("view" . ,view)
                                ("color" . ,color) ("f" . ,flags) ("zid" . ,zid)
                                ("owner" . ,owner) ("rid" . ,rid) ("path" . ,path))))))

(defun zimbra-mount (name owner-id folder-id &key (color "1") (flags "#") view)
  (let ((response (zimbra-create-mountpoint-request "1" name
                                                    :color color :view view :flags flags
                                                    :zid owner-id :rid folder-id)))
    (let ((link (first (xml-get-elements-by-tag-name response "link"))))
      (zimbra-parse-link link))))

(defstruct zimbra-link 
  rev oname ms n l rgb perm id s color rid
  zid name owner view)

(defun zimbra-parse-link (e)
  (make-zimbra-link :rev   (xml-get-attribute e "rev")
                    :oname (xml-get-attribute e "oname")
                    :ms    (xml-get-attribute e "ms")
                    :n     (xml-get-attribute e "n")
                    :l     (xml-get-attribute e "l")
                    :rgb   (xml-get-attribute e "rgb")
                    :perm  (xml-get-attribute e "perm")
                    :id    (xml-get-attribute e "id")
                    :s     (xml-get-attribute e "s")
                    :color (xml-get-attribute e "color")
                    :rid   (xml-get-attribute e "rid")
                    :zid   (xml-get-attribute e "zid")
                    :name  (xml-get-attribute e "name")
                    :owner (xml-get-attribute e "owner")
                    :view  (xml-get-attribute e "view")))
                                                    
(defun zimbra-sanitize-path (name)
  (substitute-if #\Space (complement #'alphanumericp) name))

(defun zimbra-mount-shares ()
  (dolist (share (zimbra-shares))
    (unless (zimbra-share-mid share)
      (zimbra-mount
       (concatenate 'string (or (zimbra-share-owner-name share)
                                (zimbra-share-owner-email share)) 
                    "'s" (zimbra-sanitize-path (zimbra-share-folder-path share)))
       (zimbra-share-owner-id share)
       (zimbra-share-folder-id share)
       :view (zimbra-share-view share)))))

(defun zimbra-unmount-shares ()
  (dolist (share (zimbra-shares))
    (when (zimbra-share-mid share)
      (zimbra-delete-folder (zimbra-share-mid share)))))

#+debug
(dolist (domain (zimbra-domains))
  (dolist (dlname (zimbra-distribution-lists (zimbra-domain-name domain)))
    (let ((dl (zimbra-distribution-list (zimbra-distribution-list-name dlname))))
      (dolist (member (zimbra-distribution-list-members dl))
        (let ((account (zimbra-create-account member "password")))
          (zimbra-set-account-cos account "student"))))))


(provide "zimbra")
