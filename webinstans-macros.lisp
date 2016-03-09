;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defun trace-wrap-call (&key node function parameters state-form)
  (let ((node-var (gensym "NODE"))
	(function-var (gensym "FUNCTION"))
	(parameters-var (gensym "PARAMETERS"))
	(state-var (gensym "STATE-VAR")))
    `(let ((,node-var ,node)
	   (,function-var ,function)
	   (,parameters-var ,parameters)
	   ,@(if state-form `((,state-var ,state-form))))
       (when *instans-trace*
	 ;; ,(if state-form `(logmsg "enter ~A, state ~A" ,call-var ,state-var) `(logmsg "enter ~A" ,call-var))
	 (instans-trace-add-enter *instans-trace* :node ,node-var :function ,function-var :parameters ,parameters-var :state ,state-var))
       (multiple-value-prog1 (call-next-method)
	 ,@(if state-form `((setf ,state-var ,state-form)))
	 (when *instans-trace*
	   ;; ,(if state-form `(logmsg "exit ~A, state ~A" ,call-var ,state-var) `(logmsg "exit ~A" ,call-var))
	   (instans-trace-add-exit *instans-trace* :node ,node-var :function ,function-var :parameters ,parameters-var :state ,state-var))))))

(defmacro loggingmsgs (&body body)
  `(with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
     (let ((instans::*stream-open-close-report-output* str))
       ,@body)))

