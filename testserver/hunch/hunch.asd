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
(push  "/Users/enu/aaltodsg/webinstans/" asdf:*central-registry*)
(compile-system :webinstans)
(load-system :webinstans)

(defsystem :hunch
  :description "Web INSTANS"
  :author "Esko Nuutila <enu@iki.fi>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:hunchensocket
	       :css-lite
	       :parenscript
               :alexandria
               :ironclad
               :flexi-streams
               :chunga
               :trivial-utf-8
               :trivial-backtrace
               :bordeaux-threads
               :cl-fad
	       :cl-who)
  :serial t
  :components ((:file "hunch2")))

(defpackage #:hunch
  (:use #:common-lisp ; #:instans
	) ; #:lisp-unit 
  (:import-from :instans main)
  (:import-from :hunchentoot start stop content-type* easy-acceptor *dispatch-table* create-regex-dispatcher define-easy-handler)
  ;; (:import-from :parenscript ps import-macros-from-lisp create @ getprop defpsmacro))
)


