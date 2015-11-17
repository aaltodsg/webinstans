;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defun main (args)
  (multiple-value-bind (trace-options other)
      (instans::extract-options args '("trace"))
    (instans::inform "Got tracing arguments ~S" trace-options)
    (instans::inform "Other args ~S" other)
    (when trace-options
      (when (not (= 1 (length trace-options)))
	(error "Expecting just one tracing argument in ~A" args))
      (instans-trace-init))
    (multiple-value-prog1 (instans::main (list (instans::parsed-options-to-string other)))
      (when trace-options
	(let ((trace-output-file (second (first trace-options))))
	  (cond ((null trace-output-file)
		 (instans-trace-print *instans-trace*))
		(t
		 (with-open-file (output trace-output-file :if-exists :supersede)
		   (instans-trace-print *instans-trace* output)))))))))


	    
