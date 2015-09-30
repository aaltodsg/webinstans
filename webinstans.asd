;;; -*- Mode: Common-Lisp -*-                                                                                                           

(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)
(in-package :asdf)

(push  "/Users/enu/aalto-dsg/instans/src/" asdf:*central-registry*)
(compile-system :instans)
(load-system :instans)

(defpackage #:webinstans
  (:use #:common-lisp #:instans) ; #:lisp-unit 
  (:export "" "MAIN"))

(defsystem :webinstans
  :description "Web INSTANS"
  :author "Esko Nuutila <enu@iki.fi>"
  :license "MIT"
  :version 0.0.1
  :depends-on (:hunchentoot
               :alexandria
               :ironclad
               :flexi-streams
               :chunga
               :trivial-utf-8
               :trivial-backtrace
               :bordeaux-threads
               :cl-fad)
  :serial t
  :components ((:file "webinstans-macros")
	       (:file "webinstans" :depends-on ("webinstans-macros"))))





