;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defvar *logfile* "log")

(defun logclean ()
  (if (probe-file *logfile*)
      (delete-file *logfile*)))

(defun logmsg (msg &rest args)
  (with-open-file (str *logfile* :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format str (format nil "~%~S~%" msg) args)))

(defun logdescribe (object)
  (with-open-file (str *logfile* :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((*standard-output* str))
      (describe object))))

