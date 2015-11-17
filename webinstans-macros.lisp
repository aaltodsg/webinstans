;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defmacro trace-wrap-call (call)
  (let ((call-var (gensym "CALL")))
    `(let ((,call-var ,call))
       (when *instans-trace*
	 (logmsg "enter ~A" ,call-var)
	 (instans-trace-add-enter *instans-trace* ,call-var))
       (multiple-value-prog1 (call-next-method)
	 (when *instans-trace*
	   (logmsg "exit ~A" ,call-var)
	   (instans-trace-add-exit *instans-trace* ,call-var))))))

(defmacro loggingmsgs (&body body)
  `(with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
     (let ((instans::*stream-open-close-report-output* str))
       ,@body)))
