;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defclass instans-trace ()
  ((operations :accessor instans-trace-operations :initform nil)
   (tail :accessor instans-trace-tail :initform nil)))
  
(defgeneric instans-trace-add-call (instans-trace &key call state)
  (:method ((this instans-trace) &key call state)
    (let ((new (list (if state (list :call call :state state) (list :call call)))))
      (cond ((null (instans-trace-operations this))
	     (setf (instans-trace-operations this) new)
	     (setf (instans-trace-tail this) new))
	    (t
	     (setf (cdr (instans-trace-tail this)) new)
	     (setf (instans-trace-tail this) (cdr (instans-trace-tail this)))))
      (first new))))

(defgeneric instans-trace-add-enter (instans-trace &key call state)
  (:method ((this instans-trace) &key call state)
    (instans-trace-add-call this :call (cons :enter call) :state state)))

(defgeneric instans-trace-add-exit (instans-trace &key call state)
  (:method ((this instans-trace) &key call state)
    (instans-trace-add-call this :call (cons :exit call) :state state)))

(defgeneric instans-trace-node-state (node)
  (:method ((this instans::token-store))
    (list :token-store (instans::token-store-tokens this)))
  (:method ((this instans::join-node))
    (list :alpha-index (and (instans::join-alpha-index this) (instans::index-tokens (instans::join-alpha-index this)))
	  :beta-index (and (instans::join-beta-index this) (instans::index-tokens (instans::join-beta-index this)))))
  (:method ((this instans::existence-start-node))
    (list :map (instans::token-map-map (instans::existence-start-node-token-map this))))
  (:method ((this instans::filter-with-previous-value))
    (list :map (instans::filter-with-previous-value-token-map this)))
  (:method ((this instans::aggregate-join-node))
    (list :map (instans::aggregate-join-token-group-map this)))
  (:method ((this instans::query-node))
    (and (slot-boundp this 'instans::project-index) (list :project-index (instans::solution-modifiers-project-index this))))
  (:method ((this instans::node))
    nil))

(defvar *instans-trace* nil)

(defun instans-trace-init ()
  (setf *instans-trace* (make-instance 'instans-trace)))

(defun instans-trace-print (trace &optional stream-or-file type)
  (unless type (setf type (if (streamp stream-or-file) :json (intern (string-upcase (pathname-type (pathname stream-or-file))) :keyword))))
  (flet ((output (stream)
	   (case type
	     (:json (instans-trace-print-json-stream trace stream))
	     (:lisp (format stream "~S~%" (instans-trace-operations trace)))
	     (t (error "Cannot output trace as ~S" type)))))
    (cond ((streamp stream-or-file)
	   (output stream-or-file))
	  (t
	   (with-open-file (output-stream stream-or-file :direction :output :if-exists :supersede)
	     (output output-stream))))))

(defun instans-trace-print-json-stream (trace stream)
  (labels ((print-trace-item (item)
	     (format stream "~&{")
	     (let* ((call (getf item :call))
		    (state (getf item :state)))
	       (destructuring-bind (direction op . args) call
		 (format stream "~(\"direction\": ~S~), " (string direction))
		 (format stream "~(\"operation\": ~S~), " (string op))
		 (format stream "~(\"parameters\": ~A~)" (list-to-json args)))
	       (when state
		 (format *error-output* "~%state = ~S" state)
		 (format stream ", \"state\": {")
		 (loop for comma = "" then ", "
		       for rest on state by #'cddr
		       for key = (first rest)
		       for value = (second rest)
		       do (format stream "~A~(~S: ~A~)" comma (string key) (sparql-value-to-json value :nil-as-false-p nil)))
		 (format stream "}")))
	     (format stream "~%}")))
    (format stream "~&[")
    (loop for comma = "" then ", "
	  for item in (instans-trace-operations trace)
	  do (format stream comma)
	  do (print-trace-item item))
    (format stream "~&]")))

(defun json-typed-value (type value)
  (format nil "{ \"type\": \"~A\", \"value\": ~A}" type value))

(defun sparql-value-type-as-json-string (x &key (nil-as-false-p t))
  (cond ((null x) (if nil-as-false-p "boolean" "list"))
	((typep x 'instans::xsd-string-value) "string")
	((typep x 'instans::xsd-boolean-value) "boolean")
	((typep x 'instans::xsd-integer-value) "integer")
	((typep x 'instans::xsd-decimal-value) "decimal")
	((typep x 'instans::xsd-double-value) "double")
	((typep x 'instans::xsd-datetime-value) "datetime")
	((instans::rdf-literal-p x) "literal")
	((instans::rdf-iri-p x) "iri")
	((instans::rdf-blank-node-p x) "blank")
	((instans::sparql-var-p x) "var")
	((instans::sparql-unbound-p x) "unbound")
	((instans::nodep x) "node")
	((instans::existence-start-node-token-state-p x) "existenceStartNodeTokenState")
	((keywordp x) "keyword")
	((listp x)
	 (cond ((and (consp (first x)) (null (first (first x))) (numberp (second (first x))))
		"token")
	       ((not (listp (cdr x))) "pair")
	       (t "list")))
	(t "unknown")))

(defun sparql-value-to-json (x &key (nil-as-false-p t))
  (let* ((tp nil) (r
  (let ((type (sparql-value-type-as-json-string x :nil-as-false-p nil-as-false-p)))
    (cond
	  ((eq type "token")
	   (token-to-json x))
	  ((eq type "list")
	   (list-to-json x))
	  ((eq type "pair")
	   (list-to-json (list (car x) (cdr x))))
	  (t
	   (let ((value (cond ((instans::nodep x) (format nil "\"~A\"" (node-json-name x)))
			      ((instans::sparql-var-p x) (format nil "\"~A\"" (sparql-var-json-name x)))
			      ((instans::rdf-iri-p x) (format nil "\"~A\"" (instans::sparql-value-to-string x)))
			      ((instans::existence-start-node-token-state-p x) (format nil "{\"counter\": ~D, \"isActive\": ~S}"
										       (instans::existence-start-node-token-state-counter x)
										       (if (instans::existence-start-node-token-state-active-p x) "true" "false")))
			      ((eq type "keyword")
			       (instans::sparql-value-to-string (string-downcase (string x))))
			      (t (instans::sparql-value-to-string x)))))
	     (setf tp type)
	     (json-typed-value type value))))))
	)
    (logmsg "~%sparql-value-to-json ~S (type ~S) -> ~S" x tp r)
    r))

(defun sparql-var-json-name (x)
  (string-downcase (instans::uniquely-named-object-name x)))  

(defun sparql-var-to-json (x)
  (json-typed-value "var" (format nil "\"~A\"" (sparql-var-json-name x))))

(defun checksum-to-json (x)
  (json-typed-value "checksum" (second x)))

(defun binding-to-json (x)
  (format nil "{ \"type\": \"binding\", \"var\": ~A, \"value\": ~A}" (sparql-var-to-json (first x)) (sparql-value-to-json (second x))))

(defun downcase-and-dash-to-underline (string)
  (coerce (loop for ch in (coerce string 'list)
		when (char= ch #\-) collect #\_
		else collect (char-downcase ch))
	  'string))

(defun node-json-name (n)
  (downcase-and-dash-to-underline (instans::node-name n)))

(defun list-to-json (list)
  (format nil "[~{~A~^, ~}]" (mapcar #'sparql-value-to-json list)))

(defun token-to-json (token)
  (let ((r
  (json-typed-value "token" (format nil "[~{~A~^, ~}]"
				    (mapcar #'(lambda (x)
						(cond ((and (consp x) (= 2 (length x)))
						       (cond ((null (first x)) (checksum-to-json x))
							     (t (binding-to-json x))))
						      (t x)))
					    token))))
	)
    (logmsg "~%token-to-json ~S -> ~S" token r)
    r))
