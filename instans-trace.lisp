;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defclass instans-trace ()
  ((operations :accessor instans-trace-operations :initform nil)
   (tail :accessor instans-trace-tail :initform nil)
   (current-node-states :accessor instans-trace-current-node-states :initform (make-hash-table))))
  
(defclass instans-trace-entry ()
  ((node :accessor instans-trace-entry-node :initarg :node :initform nil)
   (direction :accessor instans-trace-entry-direction :initarg :direction :initform nil)
   (function :accessor instans-trace-entry-function :initarg :function :initform nil)
   (parameters :accessor instans-trace-entry-parameters :initarg :parameters :initform nil)
   (state :accessor instans-trace-entry-state :initarg :state :initform nil)
   (delta :accessor instans-trace-entry-delta :initarg :delta :initform nil)))

(defmethod print-object ((this instans-trace-entry) stream)
  (format stream "#<~A ~A ~A ~A>" (type-of this) (instans-trace-entry-direction this) (instans-trace-entry-function this) (instans-trace-entry-parameters this) ))

(defclass instans-trace-state () ())

(defclass instans-trace-token-store-state (instans-trace-state)
  ((tokens :accessor instans-trace-token-store-state-tokens :initarg :tokens :initform nil)))

(defclass instans-trace-join-node-state (instans-trace-state)
  ((alpha-items :accessor instans-trace-join-node-state-alpha-items :initarg :alpha-items :initform nil)
   (beta-items :accessor instans-trace-join-node-state-beta-items :initarg :beta-items :initform nil)))

(defclass instans-trace-existence-start-node-state (instans-trace-state)
  ((tokens :accessor instans-trace-existence-start-node-state-tokens :initarg :tokens :initform nil)
   (map-items :accessor instans-trace-existence-start-node-state-map-items :initarg :map-items :initform nil)))

(defclass instans-trace-query-node-state (instans-trace-state)
  ((solutions :accessor instans-trace-query-node-state-solutions :initarg :solutions :initform nil)))

;; instans-trace-token-store-state

;; instans-trace-join-node-state

;; instans-trace-existence-start-node-state

;; instans-trace-query-node-state

(defclass instans-trace-delta () ())

(defclass instans-trace-token-store-delta (instans-trace-delta)
  ((added-tokens :accessor instans-trace-token-store-delta-added-tokens :initarg :added-tokens)
   (removed-tokens :accessor instans-trace-token-store-delta-removed-tokens :initarg :removed-tokens)))

(defclass instans-trace-join-node-delta (instans-trace-delta)
  ((alpha-added-items :accessor instans-trace-join-node-delta-alpha-added-items :initarg :alpha-added-items)
   (alpha-removed-items :accessor instans-trace-join-node-delta-alpha-removed-items :initarg :alpha-removed-items)
   (beta-added-items :accessor instans-trace-join-node-delta-beta-added-items :initarg :beta-added-items)
   (beta-removed-items :accessor instans-trace-join-node-delta-beta-removed-items :initarg :beta-removed-items)))

(defclass instans-trace-existence-start-node-delta (instans-trace-delta)
  ((added-tokens :accessor instans-trace-existence-start-node-delta-added-tokens :initarg :added-tokens)
   (removed-tokens :accessor instans-trace-existence-start-node-delta-removed-tokens :initarg :removed-tokens)
   (map-added-items :accessor instans-trace-existence-start-node-delta-map-added-items :initarg :map-added-items)
   (map-removed-items :accessor instans-trace-existence-start-node-delta-map-removed-items :initarg :map-removed-items)))

(defclass instans-trace-query-node-delta (instans-trace-delta)
  ((added-solutions :accessor instans-trace-query-node-delta-added-solutions :initarg :added-solutions)
   (removed-solutions :accessor instans-trace-query-node-delta-removed-solutions :initarg :removed-solutions)))

;; instans-trace-token-store-delta

;; instans-trace-join-node-delta

;; instans-trace-existence-start-node-delta

;; instans-trace-query-node-delta

(defgeneric instans-trace-state-delta (prev-state new-state)
  (:method ((this instans-trace-token-store-state) (other instans-trace-token-store-state))
    (list this other))
  (:method ((this instans-trace-join-node-state) (other instans-trace-join-node-state))
    (list this other))
  (:method ((this instans-trace-existence-start-node-state) (other instans-trace-existence-start-node-state))
    (list this other))
  (:method ((this instans-trace-query-node-state) (other instans-trace-query-node-state))
    (list this other)))

(defgeneric instans-trace-add-call (instans-trace &key node direction function parameters state)
  (:method ((this instans-trace) &key node direction function parameters state)
    ;; (logmsg "instans-trace-add-call ~A ~A, hash-table = ~A" node function (instans-trace-current-node-states this))
    (let* ((delta (and node (prog1
			     (let ((prev-state (gethash node (instans-trace-current-node-states this))))
			       (and prev-state (instans-trace-state-delta prev-state state)))
			     (setf (gethash node (instans-trace-current-node-states this)) state))))
	   (entry (make-instance 'instans-trace-entry :node node :direction direction :function function :parameters parameters :state state :delta delta))
	   (new (list entry)))
      ;; (logmsg "add-call ~A" entry)
      (describe entry)
      (cond ((null (instans-trace-operations this))
	     (setf (instans-trace-operations this) new)
	     (setf (instans-trace-tail this) new))
	    (t
	     (setf (cdr (instans-trace-tail this)) new)
	     (setf (instans-trace-tail this) (cdr (instans-trace-tail this)))))
      (first new))))

(defgeneric instans-trace-add-enter (instans-trace &key node function parameters state)
  (:method ((this instans-trace) &key node function parameters state)
    (instans-trace-add-call this :node node :direction :enter :function function :parameters parameters :state state)))

(defgeneric instans-trace-add-exit (instans-trace &key node function parameters state)
  (:method ((this instans-trace) &key node function parameters state)
    (instans-trace-add-call this :node node :direction :exit :function function :parameters parameters :state state)))

(defgeneric instans-trace-node-state (node)
  (:method ((this instans::token-store))
    (make-instance 'instans-trace-token-store-state :tokens (instans::token-store-tokens this)))
  (:method ((this instans::join-node))
    (make-instance 'instans-trace-join-node-state
		   :alpha-items (and (instans::join-alpha-index this) (instans::index-tokens (instans::join-alpha-index this)))
		   :beta-items (and (instans::join-beta-index this) (instans::index-tokens (instans::join-beta-index this)))))
  (:method ((this instans::existence-start-node))
    (make-instance 'instans-trace-existence-start-node-state
		   :tokens (instans::token-store-tokens this)
		   :map-items (instans::token-map-map (instans::existence-start-node-token-map this))))
  ;; (:method ((this instans::filter-with-previous-value))
  ;;   (list :map (instans::filter-with-previous-value-token-map this)))
  ;; (:method ((this instans::aggregate-join-node))
  ;;   (list :map (instans::aggregate-join-token-group-map this)))
  (:method ((this instans::query-node))
    (make-instance 'instans-trace-query-node-state :solutions (instans::token-store-tokens this)))
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
  ;; (labels ((print-trace-item (item)
  ;; 	     (format stream "~&{")
  ;; 	     (let* ((call (getf item :call))
  ;; 		    (state (getf item :state)))
  ;; 	       (destructuring-bind (direction op . args) call
  ;; 		 (format stream "~(\"direction\": ~S~), " (string direction))
  ;; 		 (format stream "~(\"operation\": ~S~), " (string op))
  ;; 		 (format stream "~(\"parameters\": ~A~)" (list-to-json args)))
  ;; 	       (when state
  ;; 		 (format *error-output* "~%state = ~S" state)
  ;; 		 (format stream ", \"state\": {")
  ;; 		 (loop for comma = "" then ", "
  ;; 		       for rest on state by #'cddr
  ;; 		       for key = (first rest)
  ;; 		       for value = (second rest)
  ;; 		       do (format stream "~A~(~S: ~A~)" comma (string key) (sparql-value-to-json value :nil-as-false-p nil)))
  ;; 		 (format stream "}")))
  ;; 	     (format stream "~%}")))
    (format stream "~&[")
    ;; (logmsg "before printing entries")
    (loop for comma = "" then ",~%"
	  for entry in (instans-trace-operations trace)
	  ;; do (logmsg "print entry ~A" entry)
	  do (format stream comma)
	  do (instans-trace-entry-print-json entry stream))
    (format stream "]~&"))

(defun instans-trace-type-to-json (x stream)
  (format stream "\"type\": \"~(~A~)\"" (subseq (string (type-of x)) (length "instans-trace-"))))

(defun instans-trace-slot-to-json (x slot stream)
  (let ((value (slot-value x slot)))
    (if (keywordp value) 
	(format stream "\"~(~A~)\": \"~(~A~)\"" slot value)
	(format stream "\"~(~A~)\": ~A" slot (sparql-value-to-json value :nil-as-false-p nil)))))

(defgeneric instans-trace-entry-print-json (entry stream)
  (:method ((this instans-trace-entry) stream)
    (format stream "{")
    (instans-trace-type-to-json this stream)
    (when (instans-trace-entry-node this)
      (format stream ", ")
      (instans-trace-slot-to-json this 'node stream))
    (format stream ", ")
    (instans-trace-slot-to-json this 'direction stream)
    (format stream ", ")
    (instans-trace-slot-to-json this 'function stream)
    (format stream ", ")
    (instans-trace-slot-to-json this 'parameters stream)
    (when (instans-trace-entry-state this)
      (format stream ", \"state\": ")
      (instans-trace-node-state-print-json (instans-trace-entry-state this) stream))
    (format stream "}")))

(defgeneric instans-trace-node-state-print-json (state stream)
  (:method ((this instans-trace-state) stream)
    ;; (logmsg "instans-trace-node-state-print-json ~A" this)
    (format stream "{")
    (instans-trace-type-to-json this stream)
    (loop for slot in (get-slots this)
	  do (format stream ", ")
	  do (instans-trace-slot-to-json this slot stream))
    (format stream "}~%")))

;; (defgeneric instans-trace-node-delta-print-json (delta stream)
;;   (:method ((this instans-trace-token-store-delta) stream)
;;     )
;;   (:method ((this instans-trace-join-node-delta) stream)
;;     )
;;   (:method ((this instans-trace-existence-start-node-delta) stream)
;;     )
;;   (:method ((this instans-trace-query-node-delta) stream)
;;     )
;; )

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
  (let* ((tp nil) (tokenp nil) (r
  (let ((type (sparql-value-type-as-json-string x :nil-as-false-p nil-as-false-p)))
    (cond
	  ((equal type "token")
	   (setf tokenp t)
	   (token-to-json x))
	  ((equal type "list")
	   (list-to-json x))
	  ((equal type "pair")
	   (list-to-json (list (car x) (cdr x))))
	  (t
	   (let ((value (cond ((instans::nodep x) (format nil "\"~A\"" (node-json-name x)))
			      ((instans::sparql-var-p x) (format nil "\"~A\"" (sparql-var-json-name x)))
			      ((instans::rdf-blank-node-p x) (format nil "\"~A\"" (instans::uniquely-named-object-name x)))
			      ((instans::rdf-iri-p x) (format nil "\"~A\"" (instans::sparql-value-to-string x)))
			      ((instans::existence-start-node-token-state-p x) (format nil "{\"counter\": ~D, \"isActive\": ~S}"
										       (instans::existence-start-node-token-state-counter x)
										       (if (instans::existence-start-node-token-state-active-p x) "true" "false")))
			      ((equal type "keyword")
			       (instans::sparql-value-to-string (string-downcase (string x))))
			      (t (instans::sparql-value-to-string x)))))
	     (setf tp type)
	     (json-typed-value type value))))))
	)
    ;; (logmsg "~%sparql-value-to-json ~S (type ~S), tokenp = ~S -> ~S" x tp tokenp r)
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
    ;; (logmsg "~%token-to-json ~S -> ~S" token r)
    r))
