;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defclass webinstans-server (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name)
   (instans :initform nil :accessor webinstans-server-instans))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

(defvar *webinstans-servers* (list (make-instance 'webinstans-server :name "/bongo")))

(defun find-server (request)
  (find (hunchentoot:script-name request) *webinstans-servers* :test #'string= :key #'name))
    
(pushnew 'find-server hunchensocket:*websocket-dispatch-table*)

(defun message-sample (message &optional (max-length 60))
  (cond ((<= (length message) max-length)
	 message)
	(t
	 (let ((prefix-length (ceiling max-length 2))
	       (suffix-length (floor max-length 2)))
	   (format nil "~A ... ~A" (subseq message 0 prefix-length) (subseq message (- (length message) suffix-length) (length message)))))))
  

(defvar *message-number*)

(defun broadcast (server message &rest args)
  (let ((text (apply #'format nil (format nil "~D ~A" (incf *message-number*) message) args)))
    (logmsg "broadcast ~A ~A" server (message-sample text))
    (loop for peer in (hunchensocket:clients server)
	  do (hunchensocket:send-text-message peer text))))

(defmethod hunchensocket:client-connected ((server webinstans-server) user)
  (logmsg "client-connected ~A ~A" server user)
  ;; (logdescribe server)
  ;; (logdescribe user)
  (setf *message-number* -1)
  (broadcast server "~a has joined ~a" (name user) (name server)))

(defmethod hunchensocket:client-disconnected ((server webinstans-server) user)
  (logmsg "client-disconnected ~A ~A" server user)
  ;; (logdescribe server)
  ;; (logdescribe user)
  (broadcast server "~a has left ~a" (name user) (name server)))

(defun parse-message (message)
  (let ((index (or (search " " message) (length message))))
    (values (subseq message 0 index) (subseq message (if (< index (length message)) (1+ index) index)))))

(defvar *server* nil)

(defun get-dot (server)
  ;; (logmsg "got command dot")
  ;; (logdescribe server)
  (let ((instans (webinstans-server-instans server)))
    (when instans
      (let* ((dot-file-name (instans::create-temp-file-name))
	     (svg-file-name (format nil "~A.svg" dot-file-name)))
	(with-open-file (stream dot-file-name :direction :output :if-exists :error)
	  (instans::print-dot instans :stream stream :show-vars-p nil :html-labels-p nil :binding-info-box-p t))
	(instans::shell-cmd "dot" "-Tsvg" "-O" dot-file-name)
	(let ((svg (instans::file-contents-to-string svg-file-name)))
	  (logmsg "get-dot: dot-result ~A" (message-sample svg))
	  (broadcast server "dot-result ~A" svg)
	  (delete-file dot-file-name)
	  (delete-file svg-file-name))))))

;; ((equal command "run")
;;  (logmsg "got command run")
;;  ;; (logdescribe server)
;;  (let ((instans (webinstans-server-instans server)))
;;    (when instans
;;      (loggingmsgs
;;        (instans::main "--execute" :instans instans :exit-after-processing-args-p t :execute-immediately-p t)))))

(defun get-var-mappings (server)
  (let ((instans (webinstans-server-instans server)))
    (when instans
      (let ((var-mappings (format nil "[~{~A~^, ~}]" 
			       (loop for (from . to) in (instans::instans-bindings instans)
				     collect (format nil "[~A, ~A]" (sparql-var-to-json from) (sparql-var-to-json to))))))
	(logmsg "get-var-info: ~A" var-mappings)
	(broadcast server "var-mappings ~A" var-mappings)))))

(defun get-defining-nodes (server)
  (let ((instans (webinstans-server-instans server)))
    (when instans
      (let* ((nodes (instans::instans-nodes instans))
	     (result (loop for (from . to) in (instans::instans-bindings instans)
			   collect (format nil "[\"~A\", [~{\"~A\"~^, ~}]]"
					   (sparql-var-json-name from)
					   (loop for node in nodes when (member to (instans::node-def node) :test #'instans::sparql-var-equal)
						 collect (node-json-name node))) into r
			   finally (return (format nil "[~{~A~^, ~}]" r)))))
	(logmsg "get-defining-nodes: ~A" result)
	(broadcast server "defining-nodes ~A" result)))))
  
(defun get-using-nodes (server)
  (let ((instans (webinstans-server-instans server)))
    (when instans
      (let* ((nodes (instans::instans-nodes instans))
	     (result (loop for (from . to) in (instans::instans-bindings instans)
			   collect (format nil "[\"~A\", [~{\"~A\"~^, ~}]]"
					   (sparql-var-json-name from)
					   (loop for node in nodes when (member to (instans::node-use node) :test #'instans::sparql-var-equal)
						 collect (node-json-name node))) into r
			   finally (return (format nil "[~{~A~^, ~}]" r)))))
	(logmsg "get-using-nodes: ~A" result)
	(broadcast server "using-nodes ~A" result)))))
  
(defun get-matching-nodes (server)
  (let ((instans (webinstans-server-instans server)))
    (when instans
      (let* ((nodes (instans::filter #'instans::join-node-p (instans::instans-nodes instans)))
	     (result (loop for (from . to) in (instans::instans-bindings instans)
			   collect (format nil "[\"~A\", [~{\"~A\"~^, ~}]]"
					   (sparql-var-json-name from)
					   (loop for node in nodes when (member to (instans::node-use node) :test #'instans::sparql-var-equal)
						 collect (node-json-name node))) into r
			   finally (return (format nil "[~{~A~^, ~}]" r)))))
	(logmsg "get-matching-nodes: ~A" result)
	(broadcast server "matching-nodes ~A" result)))))
  

(defun get-trace (server)
  ;; (logmsg "get-trace ~A" server)
  (let ((instans (webinstans-server-instans server)))
    (when instans
      ;; (logmsg "get-trace, before instans-trace-print ~A" server)
      (let ((json-trace (with-output-to-string (ostream) (instans-trace-print *instans-trace* ostream :json))))
	;; (logmsg "get-trace, after instans-trace-print ~A" server)
	;; (logmsg "get-trace: ~A" (message-sample json-trace))
	(logmsg "get-trace: ~A" json-trace)
	(logmsg "trace = ~A" (instans-trace-operations *instans-trace*))
	(broadcast server "trace ~A" json-trace)))))

(defmethod hunchensocket:text-message-received ((server webinstans-server) user message)
  (handler-case
      (let ((*server* server))
	(logmsg "text-message-received ~A ~A ~A" server user message)
	(multiple-value-bind (command args)
	    (parse-message message)
	  (declare (ignorable args))
	  (logmsg "text-message-received: command ~S, args ~S" command args)
	  (cond ((equal command "parameters")
		 (logmsg "got command parameters")
					;		 (setf (webinstans-server-instans server) (instans::main args :exit-after-processing-args-p nil :execute-immediately-p nil))
		 (let ((succeededp t))
		   (handler-case
		       (progn
			 (let ((trace-file (instans::create-temp-file-name :type "json")))
			   (setf args (format nil "--trace=~A ~A" trace-file args))
			   (instans-trace-init)
			   (let ((instans (instans::create-instans)))
			     (setf (webinstans-server-instans server) instans)
			     (loggingmsgs
			       (let ((*trace-output* *logstream*))
				 (handler-case
				   (progn
				     (handler-case
				     	 (progn 
				     	   (untrace)
				     	   (instans::trace-rete)
				     	   (trace instans-trace-add-call))
				       (condition () nil))
				     (webinstans::main args :instans instans))
				   (condition (e)
				     (setf succeededp nil)
				     (logmsg "webinstans::main error ~S" e)
				     (let ((*standard-output* *logstream*)
					   (*error-output* *logstream*))
				       (describe e))
				     (broadcast server "error ~S" e)
				     )))))
			   (get-dot server)
			   (get-var-mappings server)
			   (get-defining-nodes server)
			   (get-using-nodes server)
			   (get-matching-nodes server)
			   (handler-case
			       (get-trace server)
			       (condition (e)
				 (logmsg "webinstans::get-trace error ~S" e)))
			   (logmsg "Execution succeeded")
			   (broadcast server "end ~:[failed~;succeeded~]" succeededp)))
		     (condition (e)
		       (broadcast server "error ~S" e)
		       (broadcast server "end failed" e)))))
		;; ((equal command "dot")
		;;  (get-dot server))
		(t (broadcast server "error unknown command ~A" command)))))
    (t (e)
      (logmsg "text-message-received got an error ~S" e)
      (broadcast server "error text-message-received ~S" e))))



(defvar *server* nil)

(defun start-server ()
  (when *server*
    (hunchensocket::stop *server*))
  (logclean)
  (setf *server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
  (hunchensocket::start *server*))

