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

(defun json-typed-value (type value)
  (format nil "{ \"type\": \"~A\", \"value\": ~A}" type value))
  

(defun sparql-value-to-json (x)
  (let ((type (cond ((typep x 'instans::xsd-string-value) "string")
		    ((typep x 'instans::xsd-boolean-value) "boolean")
		    ((typep x 'instans::xsd-integer-value) "integer")
		    ((typep x 'instans::xsd-decimal-value) "decimal")
		    ((typep x 'instans::xsd-double-value) "double")
		    ((typep x 'instans::xsd-datetime-value) "datetime")
		    ((instans::rdf-literal-p x) "literal")
		    ((instans::rdf-iri-p x) "iri")
		    ((instans::rdf-blank-node-p x) "blank")
		    ((instans::sparql-unbound-p x) "unbound")
		    ((instans::nodep x) "node")
		    (t "null")))
	(value (cond ((instans::nodep x) (downcase-and-dash-to-underline (instans::node-name x)))
		     ((or (instans::sparql-var-p x) (instans::rdf-iri-p x)) (format nil "\"~A\"" (instans::sparql-value-to-string x)))
		     (t (instans::sparql-value-to-string x)))))
    (json-typed-value type value)))

(defun sparql-var-to-json (x)
  (json-typed-value "var" (format nil "\"~A\"" (instans::uniquely-named-object-name x))))

(defun checksum-to-json (x)
  (json-typed-value "checksum" (second x)))

(defun binding-to-json (x)
  (format nil "{ \"type\": \"binding\", \"var\": ~A, \"value\": ~A}" (sparql-var-to-json (first x)) (sparql-value-to-json (second x))))

(defun downcase-and-dash-to-underline (string)
  (coerce (loop for ch in (coerce string 'list)
		when (char= ch #\-) collect #\_
		else collect (char-downcase ch))
	  'string))


(defun list-to-json (list)
  (format nil "[~{~A~^, ~}]" (mapcar #'sparql-value-to-json list)))

(defgeneric token-to-json (node token)
  (:method ((node instans::triple-pattern-node) values)
    (declare (ignorable node))
    (list-to-json values))
  (:method ((node instans::node) token)
    (declare (ignorable node))
    (json-typed-value "token" (format nil "[~{~A~^, ~}]"
				      (mapcar #'(lambda (x)
						  (cond ((and (consp x) (= 2 (length x)))
							 (cond ((null (first x)) (checksum-to-json x))
							       (t (binding-to-json x))))
							(t x)))
					      token)))))

;; (defun opname-to-json (x)
;;   (format nil "{ \"type\": \"function\", \"value\": \"~A\"}" x))

(defmethod instans::rete-add :around ((this instans::instans) subj pred obj graph)
  (let* ((function "rete-add")
	 (args (mapcar #'sparql-value-to-json (list (instans::instans-input-processor-name instans::*current-input-processor*) subj pred obj graph)))
	 (call (format nil "~A [~{~A~^, ~}]" function args)))
    (when *room*
      (logmsg "enter ~A" call)
      (broadcast *room* "enter ~A" call))
    (multiple-value-prog1 (call-next-method)
      (when *room*
	(logmsg "exit ~A" call)
	(broadcast *room* "exit ~A" call)))))

(defmethod instans::add-token :around ((node instans::node) token &optional stack)
  (declare (ignorable stack))
  (let ((node-name (node-name-to-json node))
	(args-as-json (token-to-json node token)))
    (when *room*
      (logmsg "enter add-token [~A, ~A]" node-name args-as-json)
      (broadcast *room* "enter add-token [~A, ~A]" node-name args-as-json))
    (multiple-value-prog1 (call-next-method)
      (when *room*
	(logmsg "exit add-token [~A, ~A]" node-name args-as-json)
	(broadcast *room* "exit add-token [~A, ~A]" node-name args-as-json)))))

(defmethod instans::add-alpha-token :around ((node instans::join-node) token &optional stack)
  (declare (ignorable stack))
  (let ((node-name (node-name-to-json node))
	(args-as-json (token-to-json node token)))
    (when *room*
      (logmsg "enter add-alpha-token [~A, ~A]" node-name args-as-json)
      (broadcast *room* "enter add-alpha-token [~A, ~A]" node-name args-as-json))
    (multiple-value-prog1 (call-next-method)
      (when *room*
	(logmsg "exit add-alpha-token [~A, ~A]" node-name args-as-json)
	(broadcast *room* "exit add-alpha-token [~A, ~A]" node-name args-as-json)))))

(defmethod instans::add-beta-token :around ((node instans::join-node) token &optional stack)
  (declare (ignorable stack))
  (let ((node-name (node-name-to-json node))
	(args-as-json (token-to-json node token)))
    (when *room*
      (logmsg "enter add-beta-token [~A, ~A]" node-name args-as-json)
      (broadcast *room* "enter add-beta-token [~A, ~A]" node-name args-as-json))
    (multiple-value-prog1 (call-next-method)
      (when *room*
	(logmsg "exit add-beta-token [~A, ~A]" node-name args-as-json)
	(broadcast *room* "exit add-beta-token [~A, ~A]" node-name args-as-json)))))

(defun get-dot (room)
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

;; ((equal command "run")
;;  (logmsg "got command run")
;;  ;; (logdescribe room)
;;  (let ((instans (chat-room-instans room)))
;;    (when instans
;;      (loggingmsgs
;;        (instans::main "--execute" :instans instans :exit-after-processing-args-p t :execute-immediately-p t)))))

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
		       (setf (chat-room-instans room) (instans::main args))
		       (get-dot room)
		       (logmsg "Execution succeeded")
		       (broadcast room "end succeeded"))
		   (t (e)
		     (logmsg "instans::main error ~S" e)
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
