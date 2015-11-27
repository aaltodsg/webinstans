;;; -*- Mode: Common-Lisp -*-                                                                                                           

(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)
(in-package :asdf)

(ql:quickload "hunchentoot")
(ql:quickload "hunchensocket")
;; (ql:quickload "parenscript")
;; (ql:quickload "css-lite")
;; (ql:quickload "cl-who")
(push  "/Users/enu/aaltodsg/instans/src/" asdf:*central-registry*)
(compile-system :instans)
(load-system :instans)

(defsystem :webinstans
  :description "Web INSTANS"
  :author "Esko Nuutila <enu@iki.fi>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (
	       :hunchentoot
	       :hunchensocket
	       ;; :css-lite
	       ;; :parenscript
               ;; :alexandria
               ;; :ironclad
               ;; :flexi-streams
               ;; :chunga
               ;; :trivial-utf-8
               ;; :trivial-backtrace
               ;; :bordeaux-threads
               ;; :cl-fad
	       ;; :cl-who
	       )
  :serial t
  :components ((:file "webinstans-macros")
	       (:file "util" :depends-on ("webinstans-macros"))
	       (:file "instans-trace" :depends-on ("webinstans-macros"))
	       (:file "interpreter-around-methods" :depends-on ("webinstans-macros"))
	       (:file "webinstans-server" :depends-on ("webinstans-macros"))))

(defpackage #:webinstans
  (:use #:common-lisp #:instans) ; #:lisp-unit 
  ;; (:import-from :hunchentoot start stop content-type* easy-acceptor *dispatch-table* create-regex-dispatcher define-easy-handler)
  ;; (:import-from :parenscript ps import-macros-from-lisp create @ getprop defpsmacro)
  (:export "" "MAIN"))



