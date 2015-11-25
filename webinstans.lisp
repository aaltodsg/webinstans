;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defun tracing-main (args)
  (cond ((null args)
	 (setf args (list "")))
	((stringp args)
	 (setf args (list args))))
  (multiple-value-bind (trace-options other)
      (instans::extract-options args '("trace"))
    ;; (instans::inform "Got tracing arguments ~S" trace-options)
    ;; (instans::inform "Other args ~S" other)
    (logclean)
    (when trace-options
      (when (not (= 1 (length trace-options)))
	(error "Expecting just one tracing argument in ~A" args))
      (instans-trace-init))
    (let ((main-options (instans::parsed-options-to-string other)))
      ;; (instans::inform "Haa ~S" main-options)
      (multiple-value-prog1
	  (instans::main main-options)
	(when trace-options
	  (instans-trace-print *instans-trace* (second (first trace-options))))))))

	    
