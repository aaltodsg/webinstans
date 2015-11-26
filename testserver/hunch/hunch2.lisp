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
  

(defvar *message-number*)

(defun broadcast (room message &rest args)
  (let ((text (apply #'format nil (format nil "~D ~A" (incf *message-number*) message) args)))
    (logmsg "broadcast ~A ~A" room (message-sample text))
    (loop for peer in (hunchensocket:clients room)
	  do (hunchensocket:send-text-message peer text))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (logmsg "client-connected ~A ~A" room user)
  ;; (logdescribe room)
  ;; (logdescribe user)
  (setf *message-number* -1)
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

(defun get-dot (room)
  ;; (logmsg "got command dot")
  ;; (logdescribe room)
  (let ((instans (chat-room-instans room)))
    (when instans
      (let ((dot-file-name (instans::create-temp-file-name)))
	(with-open-file (stream dot-file-name :direction :output :if-exists :error)
	  (instans::print-dot instans :stream stream :show-vars-p nil :html-labels-p nil))
	(instans::shell-cmd "dot" "-Tsvg" "-O" dot-file-name)
	(let ((svg (instans::file-contents-to-string (format nil "~A.svg" dot-file-name))))
	  (logmsg "get-dot: dot-result ~A" (message-sample svg))
	  ;; (delete-file svg)
	  ;; (delete-file dot-file-name)
	  (broadcast room "dot-result ~A" svg))))))

;; ((equal command "run")
;;  (logmsg "got command run")
;;  ;; (logdescribe room)
;;  (let ((instans (chat-room-instans room)))
;;    (when instans
;;      (loggingmsgs
;;        (instans::main "--execute" :instans instans :exit-after-processing-args-p t :execute-immediately-p t)))))

(defun get-var-mappings (room)
  (let ((instans (chat-room-instans room)))
    (when instans
      (let ((var-mappings (format nil "[~{~A~^, ~}]" 
			       (loop for (from . to) in (instans::instans-bindings instans)
				     collect (format nil "[~A, ~A]" (sparql-var-to-json from) (sparql-var-to-json to))))))
	(logmsg "get-var-info: ~A" var-mappings)
	(broadcast room "var-mappings ~A" var-mappings)))))

(defun get-defining-nodes (room)
  (let ((instans (chat-room-instans room)))
    (when instans
      (let* ((nodes (instans::instans-nodes instans))
	     (result (loop for (from . to) in (instans::instans-bindings instans)
			   collect (format nil "[\"~A\", [~{\"~A\"~^, ~}]]"
					   (sparql-var-json-name from)
					   (loop for node in nodes when (member to (instans::node-def node) :test #'instans::sparql-var-equal)
						 collect (node-json-name node))) into r
			   finally (return (format nil "[~{~A~^, ~}]" r)))))
	(logmsg "get-defining-nodes: ~A" result)
	(broadcast room "defining-nodes ~A" result)))))
  
(defun get-using-nodes (room)
  (let ((instans (chat-room-instans room)))
    (when instans
      (let* ((nodes (instans::instans-nodes instans))
	     (result (loop for (from . to) in (instans::instans-bindings instans)
			   collect (format nil "[\"~A\", [~{\"~A\"~^, ~}]]"
					   (sparql-var-json-name from)
					   (loop for node in nodes when (member to (instans::node-use node) :test #'instans::sparql-var-equal)
						 collect (node-json-name node))) into r
			   finally (return (format nil "[~{~A~^, ~}]" r)))))
	(logmsg "get-using-nodes: ~A" result)
	(broadcast room "using-nodes ~A" result)))))
  
(defun get-matching-nodes (room)
  (let ((instans (chat-room-instans room)))
    (when instans
      (let* ((nodes (instans::filter #'instans::join-node-p (instans::instans-nodes instans)))
	     (result (loop for (from . to) in (instans::instans-bindings instans)
			   collect (format nil "[\"~A\", [~{\"~A\"~^, ~}]]"
					   (sparql-var-json-name from)
					   (loop for node in nodes when (member to (instans::node-use node) :test #'instans::sparql-var-equal)
						 collect (node-json-name node))) into r
			   finally (return (format nil "[~{~A~^, ~}]" r)))))
	(logmsg "get-matching-nodes: ~A" result)
	(broadcast room "matching-nodes ~A" result)))))
  

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
					;		 (setf (chat-room-instans room) (instans::main args :exit-after-processing-args-p nil :execute-immediately-p nil))
		 (handler-case
		     (progn
		       (let ((trace-file (instans::create-temp-file-name :type "json")))
			 (setf args (format nil "--trace=~A ~A" trace-file args))
			 (setf (chat-room-instans room) (webinstans::main args))
			 (get-dot room)
			 (get-var-mappings room)
			 (get-defining-nodes room)
			 (get-using-nodes room)
			 (get-matching-nodes room)
			 (get-trace room)
			 (logmsg "Execution succeeded")
			 (broadcast room "end succeeded")))
		   (t (e)
		     (logmsg "webinstans::main error ~S" e)
		     (get-dot room)
		     (broadcast room "end failed ~S" e))))
		;; ((equal command "dot")
		;;  (get-dot room))
		(t (broadcast room "error unknown command ~A" command)))))
    (t (e)
      (logmsg "text-message-received got an error ~S" e)
      (broadcast room "error text-message-received ~S" e))))



(defvar *server* nil)

(defun start-server ()
  (when *server*
    (stop *server*))
  (setf *server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
  (start *server*))
