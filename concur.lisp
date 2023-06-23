;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Concur API Client
;;; Copyright (c) 2023 M. Brent Harp


(eval-when
 (:compile-toplevel)
 (load "config"))


(require "json" #.(translate-logical-pathname "config:lib;lisp;json.fas"))
(require "http" #.(translate-logical-pathname "config:lib;lisp;http.fas"))


(defvar *concur-protocol* "https")
(defvar *concur-host* nil)
(defvar *concur-port* 443)
(defvar *concur-debug* nil)
(defvar *concur-api-response* nil)
(defvar *concur-access-token* nil)
(defvar *concur-config* #.(translate-logical-pathname "config:etc;concur.conf"))
(defvar *concur-auth-cookie* "CONCURAUTH")


;; =====================================================================
;; LOAD DEFAULT CONFIG
;; =====================================================================

;(load *concur-config*)


;; =====================================================================
;; API
;; =====================================================================

(defun concur-api (url &key (protocol *concur-protocol*) (host *concur-host*)
		       (port *concur-port*) (access-token *concur-access-token*)
		       (body '())
		       (debug *concur-debug*))
  (let ((*standard-input*
         (http url :protocol protocol :host host :port port :debug debug
               :body (with-output-to-string 
                       (*standard-output*)
                       (json-print body)))))
    ;; Parse response
    (setq *concur-api-response* (json-value))
    (close *standard-input*)
    (when debug (format t "CONUR: DEBUG: API-RESPONSE: ~s~%" *concur-api-response*))
    *concur-api-response*))


;; =====================================================================
;; Authentication
;; =====================================================================

(defun concur-token (client-id client-secret redirect-uri code grant-type)
  (http "/oauth2/v0/token" :protocol *concur-protocol* :host *concur-host* :port *concur-port*
	:body (format nil "client_id=~a&client_secret=~a&redirect_uri=~a&code=~a&grant_type=~a"
		      client-id client-secret redirect-uri code grant-type)))

			    
