;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defmacro $$ ((selector event-binding) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda () ,@body)))

(import-macros-from-lisp '$$)

