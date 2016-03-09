;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defmacro trace-wrap-call (call &key state-form)
  (let ((call-var (gensym "CALL"))
	(state-var (gensym "STATE")))
    `(let ((,call-var ,call)
	   ,@(if state-form `((,state-var ,state-form))))
       (when *instans-trace*
	 ;; ,(if state-form `(logmsg "enter ~A, state ~A" ,call-var ,state-var) `(logmsg "enter ~A" ,call-var))
	 (instans-trace-add-enter *instans-trace* :call ,call-var ,@(if state-form `(:state ,state-form))))
       (multiple-value-prog1 (call-next-method)
	 ,@(if state-form `((setf ,state-var ,state-form)))
	 (when *instans-trace*
	   ;; ,(if state-form `(logmsg "exit ~A, state ~A" ,call-var ,state-var) `(logmsg "exit ~A" ,call-var))
	   (instans-trace-add-exit *instans-trace* :call ,call-var ,@(if state-form `(:state ,state-form))))))))

(defmacro loggingmsgs (&body body)
  `(with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
     (let ((instans::*stream-open-close-report-output* str))
       ,@body)))

