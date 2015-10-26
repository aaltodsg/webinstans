;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defvar *server* nil)

(defun create-server ()
  (setf *server* (make-instance 'easy-acceptor :port 4242)))

(defun start-server ()
  ;; (setq *dispatch-table* (list (create-regex-dispatcher "^/index" 'webinstans-index)
  ;; 					   (create-regex-dispatcher"^/configure" 'webinstans-configurator)))
  (when *server* (stop-server))
  (create-server)
  (start *server*))

(defun stop-server ()
  (stop *server*)
  (setf *server* nil))

;; (defun configurator-css ()
;;   (let ((border "1px solid #ccc"))
;;     (css-lite:css
;;      (("body")
;;       (:width "70%" :margin "0 auto" :font-family "sans-serif" :border-left border :border-right border :border-bottom border))
;;      (("h1")
;;       (:font-size "140%" :text-align "center"))
;;      (("h2")
;;       (:color "#000" :background-color "#cef" :margin "0 auto" :padding "4px 0"))
;;      (("#header")
;;       (:background-color "#cef" :padding "8px"))
;;      (("#header .logo")
;;       (:display "block" :margin "0 auto"))
;;      (("#header .strapline")
;;       (:display "block" :text-align "center" :font-size "80%" :font-style "italic")))))

(defun configurator-css ()
  (let (;(border "1px solid #ccc")
	)
    (css-lite:css
      (("#tabs")
       (:font-size "14px"))
      ;; ((".ui-widget-header")
      ;;  (:background "#b9cd6d" :border "1px solid #b9cd6d" :color "#FFFFFF" :font-weight "bold"))
      )))

(defpsmacro stringify (&rest things)
  (if (and (= (length things) 1) (stringp (car things)))
      (car things)
      `(chain (list ,@things) (join ""))))

(defpsmacro callchain (object &rest calls-and-params-list)
  (loop for rest on calls-and-params-list by #'cddr
        for call = (first rest)
        for params = (second rest)
        do (setf object `((@ ,object ,call) ,@params))
        finally (return object)))

(defclass instans-connection (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this connection!") :reader name))
  (:default-initargs :client-class 'user))

(defclass instans-user (hunchensocket:websocket-client)
  ((name :initarg :name :reader name :initform (error "Name this user!"))))

(defun broadcast (connection message &rest args)
  (loop for peer in (hunchensocket:clients connection)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((connection instans-connection) user)
  (broadcast connection "~a has joined ~a" (name user) (name connection)))

(defmethod hunchensocket:client-disconnected ((connection instans-connection) user)
  (broadcast connection "~a has left ~a" (name user) (name connection)))

(defmethod hunchensocket:text-message-received ((connection instans-connection) user message)
  (broadcast connection "~a says ~a" (name user) message))

(defvar *instans-web-servers* nil)

(defclass instans-web-server ()
  ((acceptor :accessor instans-web-server-acceptor :initarg :acceptor)
   (instans :accessor instans-web-server-instans :initarg :instans :initform nil)
   (connection :accessor instans-web-server-connection :initarg :connection :initform nil)))

(defvar *instans-connections* nil)

;; (defun find-instans (request)
;;   (find (hunchentoot:script-name request) *instans-connections* :test #'string= :key #'name))

(defun logmsg (msg)
  (with-open-file (str "log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format str "~%~A~%" msg)))

(defun try-create-and-start-instans-web-server (port)
  (let ((acceptor (make-instance 'hunchensocket:websocket-acceptor :port port)))
    (logmsg (format nil "try-create-and-start-instans-web-server ~D" port))
    (hunchentoot:start acceptor)
    (let ((server (make-instance 'instans-web-server :acceptor acceptor :connection nil ; (make-instance 'instans-connection :name (format nil "/instans~D" port))
				 )))
      (push server *instans-web-servers*)
;      (pushnew 'find-instans hunchensocket:*websocket-dispatch-table*)
      server)))

(defun create-and-start-instans-web-server ()
  (logmsg "create-and-start-instans-web-server")
  (loop for port from 2048
	do (handler-case
	       (let ((server (try-create-and-start-instans-web-server port)))
		 (logmsg (format nil "create-and-start-instans-web-server ~A" server))
		 (return-from create-and-start-instans-web-server server))
	     (t ()))))

(define-easy-handler (home-page :uri "/configure") ()
  (setf (content-type*) "text/html")
  (let* ((split-command-cases (loop for case in instans::*instans-command-cases*
	 		        collect (instans::split-command-case-parameters case)))
	 (instans-options (loop for (name properties) in split-command-cases
				for usage-text = (format nil "~{~A~^ ~}" (getf properties :usage-texts))
				nconc (loop for option in (getf properties :expanded-options)
					    collect (list name option usage-text))))
	 (select-options (cons (list :option "Select option")
			       (loop for (casename option usage-text) in instans-options
				     for option-name = (first option)
				     for option-type = (second option)
				     when (stringp option-name)
				     collect `(:option :type ,(if (eq option-type :file) "file" "any") ,(format nil "~A~@[ [~A]~]" (subseq (first option) 2) (and (symbolp option-type) option-type))))))
	 (select-options-html (eval `(cl-who::with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
				       (:select :id "options-select" :style "visibility: hidden" ,@select-options))))
;;  	 (add-parameter (ps (defun add-parameter ()
;; ;			      (alert ($ "#parameters"))
;; 			      (let* ((paramcount (getprop ($ "#parameters") 'length))
;; 				     (name (concatenate 'string "param" paramcount))
;; 				     (id (concatenate 'string "#param" paramcount)))
;; 				;; ((@
;; 				;;   ((@
;; 				;;     ((@
;; 				;;       ((@
;; 				;; 	((@
;; 				;; 	  ((@
;; 				;; 	    ((@ ($ "#parameters") append) "<LI></LI>")
;; 				;; 	    find) "li")
;; 				;; 	  attr) "id" name)
;; 				;; 	append) "<input type=\"text\"/ size=\"50\">")
;; 				;;       prepend) ((@ ($ "#options-select") clone)))
;; 				;;     find) "select")
;; 				;;   attr) "style" "visibility: visible")
;; 				(callchain ($ "#parameters") append ("<LI></LI>")
;; 					   find ("li") attr ("id" name) append ("<input type=\"text\"/ size=\"50\">") prepend (((@ ($ "#options-select") clone))) find ("select") attr ("style" "visibility: visible")
;; 					   )

;;     ;; $( '#' + name ).change(function() {
;;     ;;   $('<span>Your input here</span>').insertAfter('#' + name);
;;     ;; });

;; 				((@ ($ id) change)
;; 				 (lambda ()
;; 				   (let ((s (concatenate 'string id "select option:selected")))
;; 				     ((@ ($ s) each) (lambda ()
;; 							  (let ((type ((@ ($ this) attr) "type")))
;; 							    (if (= type "file")
;; 								(alert ((@ ($ this) text))))))))
;; 				))))))
	 (instans-web-server (create-and-start-instans-web-server))
	 (port (hunchentoot::acceptor-port (instans-web-server-acceptor instans-web-server)))
	 ;; (port 24567)
	 (head (cl-who::with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
		 (:head 
		  (:meta :charset "utf-8")
		  (:title "webINSTANS")
		  (:link :href "http://code.jquery.com/ui/1.10.4/themes/smoothness/jquery-ui.css" :rel "stylesheet")
		  (:link :href "http://www.cs.hut.fi/~enu/service.flextabledit.jquery.css" :rel "stylesheet")
		  (:script :src "http://code.jquery.com/jquery-1.10.2.js")
		  (:script :src "http://code.jquery.com/ui/1.10.4/jquery-ui.js")
		  (:script :src "http://www.cs.hut.fi/~enu/service.flextabledit.jquery.min.js")
		  (:script (cl-who:str (eval `(ps (ps:var ws-uri ,(format nil "ws://localhost:~D//instans~D" port port))))))
		  (:script :src "http://www.cs.hut.fi/~enu/socket.js")
		  (:style (cl-who:str (configurator-css)))
;		  (:script (cl-who::str add-parameter))
		  (:script (cl-who:str
			    (eval `(ps ($ (lambda ()
					    ((@ ($ "#tabs") tabs)
					     (create :active 0))
					    (callchain ($ "#paramform") submit ((lambda (event)
										  (let ((params (callchain ($ "#parameters") val)))
										    (alert (concatenate 'string "About to launch INSTANS with parameters " params " on port " ,(format nil "~D" port)))))))
					    ))))))
		  )))
	 (body (cl-who::with-html-output-to-string (*standard-output* nil :indent t)
		 (:body
;		  (cl-who::str select-options-html)
		  (:div :id "tabs"
			(:ul
			 (:li (:a :href "#configure-tab" "Configure"))
			 (:li (:a :href "#execute-tab" "Execute")))
			(:div :id "configure-tab"
			      (:form :id "paramform"
				     :action ""
				     (:textarea :name "parameters" :id "parameters" :rows 4 :cols 80)
				     (:input :type "submit" :value "Launch")
				     ;; (:fieldset
				     ;;  (:ol :id "parameters"))
				     )
			      (:div :id "messages"))
			(:div :id "execute-tab" (:p "Execute"))))))
	 (output (cl-who::with-html-output-to-string (*standard-output* nil :prologue t :indent t)
		   (:html :xmlns "http://www.w3.org/1999/xhtml"
			  :xml\:lang "en" 
			  :lang "en"
			  (cl-who::str head)
			  (cl-who::str body)))))
    (with-open-file (ostr "output" :direction :output :if-exists :supersede)
      ;; (format ostr "~%output:~%~A" output)
      ;; (format ostr "~%icc: ~%~S" instans::*instans-command-cases*)
      ;; (format ostr "~%split-command-cases:~%~S" split-command-cases)
      ;; (format ostr "~%instans-options:~%~S" instans-options)
      (format ostr "~%select-options-html:~%~S" select-options-html)
;      (format ostr "~%add-parameter:~%~S" add-parameter)
      )
    output))
