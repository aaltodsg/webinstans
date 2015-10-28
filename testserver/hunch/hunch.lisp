;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :hunch)

(defun logmsg (msg &rest args)
  (with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format str (format nil "~%~A~%" msg) args)))

(defun logdescribe (object)
  (with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((*standard-output* str))
      (describe object))))

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

(defun broadcast (room message &rest args)
  (logmsg "broadcast ~A ~A ~A" room message args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

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

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (handler-case
      (progn
	(logmsg "text-message-received ~A ~A ~A" room user message)
	(multiple-value-bind (command args)
	    (parse-message message)
	  (declare (ignorable args))
	  (logmsg "text-message-received: command ~S, args ~S" command args)
	  (cond ((equal command "parameters")
		 (logmsg "got command parameters")
		 (setf (chat-room-instans room) (instans::main2 args :exit-after-processing-args-p nil :execute-immediately-p nil)))
		((equal command "dot")
		 (logmsg "got command dot")
		 (logdescribe room)
		 (let ((instans (chat-room-instans room)))
		   (when instans
		     (let ((dot-file-name (instans::create-temp-file-name)))
		       (with-open-file (stream dot-file-name :direction :output :if-exists :error)
			 (instans::print-dot instans :stream stream :show-vars-p nil :html-labels-p nil))
		       (instans::shell-cmd "dot" "-Tsvg" "-O" dot-file-name)
		       (let ((svg (instans::file-contents-to-string (format nil "~A.svg" dot-file-name))))
			 (logmsg "text-message-received: broadcast dot-result ~A" svg)
		       (broadcast room "dot-result ~A" svg))))))
		
		;; 	   (broadcast room
		;; 		      "dot-result digraph { a -> b; }"))
		;; "dot-result digraph rete {
		;;   { rank=same; 
		;;     triple_pattern_node2[class=\"TRIPLE-PATTERN-NODE\" id=\"triple_pattern_node2\",color=Black, shape=oval, label=<&tau;<sub><font point-size=\"10\">2</font></sub>: ?0 ?1 ?2>, margin=0, tooltip=\"TRIPLE-PATTERN-NODE2: 
		;; def (?S ?P ?O)
		;; vars-out (?S ?P ?O)
		;; [?0 ?1 ?2]\"];
		;;   }
		;;   { rank=same; 
		;;     alpha_memory3[class=\"ALPHA-MEMORY\" id=\"alpha_memory3\",color=Black, shape=oval, label=<&alpha;<sub>m</sub><sub><font point-size=\"10\">3</font></sub>>, margin=0, tooltip=\"ALPHA-MEMORY3: 
		;; vars-in (?S ?P ?O)
		;; vars-out (?S ?P ?O)\"];
		;;   }
		;;   {
		;;     beta_memory1[class=\"BETA-MEMORY\" id=\"beta_memory1\",color=Black, shape=oval, label=<&beta;<sub>m</sub><sub><font point-size=\"10\">1</font></sub>>, margin=0, tooltip=\"BETA-MEMORY1: \"];
		;;     join_node4[class=\"JOIN-NODE\" id=\"join_node4\",color=Black, shape=oval, label=<J<sub><font point-size=\"10\">4</font></sub>>, margin=0, tooltip=\"JOIN-NODE4: 
		;; def (?S ?P ?O)
		;; vars-out (?S ?P ?O)\"];
		;;     bind_node5[class=\"BIND-NODE\" id=\"bind_node5\",color=Black, shape=oval, label=<B[?3]<sub><font point-size=\"10\">5</font></sub>>, margin=0, tooltip=\"BIND-NODE5: 
		;; def (?Z)
		;; use (?O)
		;; vars-in (?S ?P ?O)
		;; vars-out (?S ?P ?O ?Z)
		;; (LAMBDA (?2) (%+% ?2 10))\"];
		;;     solution_modifiers_node6[class=\"SOLUTION-MODIFIERS-NODE\" id=\"solution_modifiers_node6\",color=Black, shape=oval, label=<SM<sub><font point-size=\"10\">6</font></sub>>, margin=0, tooltip=\"SOLUTION-MODIFIERS-NODE6: 
		;; use (?Z)
		;; vars-in (?S ?P ?O ?Z)
		;; vars-out (?S ?P ?O ?Z)\"];
		;;     select_node7[class=\"SELECT-NODE\" id=\"select_node7\",color=Black, shape=oval, label=<S<sub><font point-size=\"10\">7</font></sub>>, margin=0, tooltip=\"SELECT-NODE7: 
		;; vars-in (?S ?P ?O ?Z)
		;; vars-out (?S ?P ?O ?Z)\"];
		;;   }
		;;   triple_pattern_node2 -> alpha_memory3[label=<>];
		;;   beta_memory1 -> join_node4:nw[label=<>];
		;;   alpha_memory3 -> join_node4:ne[label=<>];
		;;   join_node4 -> bind_node5[label=<>];
		;;   bind_node5 -> solution_modifiers_node6[label=<>];
		;;   solution_modifiers_node6 -> select_node7[label=<>];
		;; }\""))
		(t (broadcast room "error: command ~A unknown" command)))))
    (t (e) (logmsg "text-message-received got an error ~S" e))))



(defvar *server* nil)

(defun start-server ()
  (when *server*
    (stop *server*))
  (setf *server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
  (start *server*))
