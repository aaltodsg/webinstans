;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(hunchentoot:define-easy-handler (say-yo :uri "/configure") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defvar *server* nil)

(defun start-server ()
  (push (create-prefix-dispatcher "/configure" 'retro-games) *dispatch-table*)
  (hunchentoot:start (setf *server* (make-instance 'hunchentoot:easy-acceptor :port 4242))))

(defun stop-server ()
  (stop-server *server*))

    

