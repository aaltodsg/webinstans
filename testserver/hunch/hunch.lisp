;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :hunch)

(defun logmsg (msg &rest args)
  (with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format str (format nil "~%~A~%" msg) args)))

(defun logdescribe (object)
  (with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((*standard-output* str))
      (describe object))))

(defmacro loggingmsgs (&body body)
  `(with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
     (let ((instans::*stream-open-close-report-output* str))
       ,@body)))

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name)
   (instans :initform nil :accessor chat-room-instans))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/bongo")
                           (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))
    

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(defun message-sample (message &optional (max-length 60))
  (cond ((<= (length message) max-length)
	 message)
	(t
	 (let ((prefix-length (ceiling max-length 2))
	       (suffix-length (floor max-length 2)))
	   (format nil "~A ... ~A" (subseq message 0 prefix-length) (subseq message (- (length message) suffix-length) (length message)))))))
  

(defun broadcast (room message &rest args)
  (let ((text (apply #'format nil message args)))
    (logmsg "broadcast ~A ~A" room (message-sample text))
    (loop for peer in (hunchensocket:clients room)
	  do (hunchensocket:send-text-message peer text))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (logmsg "client-connected ~A ~A" room user)
  ;; (logdescribe room)
  ;; (logdescribe user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (logmsg "client-disconnected ~A ~A" room user)
  ;; (logdescribe room)
  ;; (logdescribe user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defun parse-message (message)
  (let ((index (or (search " " message) (length message))))
    (values (subseq message 0 index) (subseq message (if (< index (length message)) (1+ index) index)))))

(defvar *room* nil)

(defmethod instans::rete-add :around ((this instans::instans) subj pred obj graph)
  (let ((s (instans::sparql-value-to-string subj))
	(p (instans::sparql-value-to-string pred))
	(o (instans::sparql-value-to-string obj))
	(g (instans::sparql-value-to-string graph)))
    (when *room*
      (logmsg "enter rete-add ~A ~A ~A ~A" s p o g)
      (broadcast *room* "enter rete-add ~A ~A ~A ~A" s p o g))
    (multiple-value-prog1 (call-next-method)
      (when *room*
	(logmsg "exit rete-add ~A ~A ~A ~A" s p o g)
	(broadcast *room* "exit rete-add ~A ~A ~A ~A" s p o g)))))

(defmethod instans::add-token :around ((node instans::node) token &optional stack)
  (declare (ignorable stack))
  (let ((node-name (downcase-and-dash-to-underline (instans::node-name node)))
	(args-as-json (tracing-token-to-string node token)))
    (when *room*
      (logmsg "enter add-token ~A ~A" node-name args-as-json)
      (broadcast *room* "enter add-token ~A ~A" node-name args-as-json))
    (multiple-value-prog1 (call-next-method)
      (when *room*
	(logmsg "exit add-token ~A ~A" node-name args-as-json)
	(broadcast *room* "exit add-token ~A ~A" node-name args-as-json)))))

(defun downcase-and-dash-to-underline (string)
  (coerce (loop for ch in (coerce string 'list)
		when (char= ch #\-) collect #\_
		else collect (char-downcase ch))
	  'string))

(defgeneric tracing-token-to-string (node token)
  (:method ((node instans::triple-pattern-node) values)
    (declare (ignorable node))
    (let ((graph (car values))
	  (args (cdr values)))
      (format nil "[~A, ~{~A~^, ~}]" (if (null graph) "Default" (instans::sparql-value-to-string graph))
	      (mapcar #'instans::sparql-value-to-string args))))
  (:method ((node instans::node) token)
    (declare (ignorable node))
    (format nil "[~{~A~^, ~}]" (mapcar #'(lambda (x)
					   (cond ((and (consp x) (= 2 (length x)))
						  (format nil "[~A, ~A]"
							  (cond ((null (first x)) "Checksum")
								((instans::sparql-var-p (first x))
								 (instans::uniquely-named-object-name (first x)))
								(t
								 (first x)))
							  (instans::sparql-value-to-string (second x))))
						 (t x)))
				       token))))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (handler-case
      (let ((*room* room))
	(logmsg "text-message-received ~A ~A ~A" room user message)
	(multiple-value-bind (command args)
	    (parse-message message)
	  (declare (ignorable args))
	  (logmsg "text-message-received: command ~S, args ~S" command args)
	  (cond ((equal command "parameters")
		 (logmsg "got command parameters")
		 (setf (chat-room-instans room) (instans::main args :exit-after-processing-args-p nil :execute-immediately-p nil)))
		((equal command "dot")
		 (logmsg "got command dot")
		 ;; (logdescribe room)
		 (let ((instans (chat-room-instans room)))
		   (when instans
		     (let ((dot-file-name (instans::create-temp-file-name)))
		       (with-open-file (stream dot-file-name :direction :output :if-exists :error)
			 (instans::print-dot instans :stream stream :show-vars-p nil :html-labels-p nil))
		       (instans::shell-cmd "dot" "-Tsvg" "-O" dot-file-name)
		       (let ((svg (instans::file-contents-to-string (format nil "~A.svg" dot-file-name))))
			 (logmsg "text-message-received: dot-result ~A" (message-sample svg))
			 ;; (delete-file svg)
			 ;; (delete-file dot-file-name)
			 (broadcast room "dot-result ~A" svg))))))
		((equal command "run")
		 (logmsg "got command run")
		 ;; (logdescribe room)
		 (let ((instans (chat-room-instans room)))
		   (when instans
		     (loggingmsgs
		       (instans::main "--execute" :instans instans :exit-after-processing-args-p t :execute-immediately-p t)))))
		(t (broadcast room "error: command ~A unknown" command)))))
    (t (e) (logmsg "text-message-received got an error ~S" e))))



(defvar *server* nil)

(defun start-server ()
  (when *server*
    (stop *server*))
  (setf *server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
  (start *server*))
