;;; -*- Mode: Common-Lisp -*-                                                                                                           

(in-package :webinstans)

(defmethod instans::rete-add :around ((this instans::instans) subj pred obj graph)
  (trace-wrap-call (cons :rete-add (list (instans::instans-input-processor-name instans::*current-input-processor*) subj pred obj (if graph graph :default)))))

(defmethod instans::rete-remove :around ((this instans::instans) subj pred obj graph)
  (trace-wrap-call (cons :rete-remove (list (instans::instans-input-processor-name instans::*current-input-processor*) subj pred obj (if graph graph :default)))))

;; (defgeneric rete-add-rule-instance (instans node token))

;; (defgeneric rete-remove-rule-instance (instans node token))

;; (defun initialize-stores-and-indices (instans))

;; (defgeneric create-stores-and-indices (node))

;; (defun initialize-data (instans))

;; (defgeneric add-initial-data (node))

;; (defun dominator-nodes (nodes))

;; (defun clear-instans-contents (instans))

;; (defgeneric initialize-execution (instans))

;; (defgeneric initialize-constant-iris (instans))

;; (defgeneric initialize-constant-literals (instans))

;; (defgeneric initialize-datablock-nodes (instans))

;; (defgeneric datablock-tokens (node))

;; (defun run-instanses (instanses))

;; (defgeneric instans-runnable-input-processors (instans))

;; (defgeneric run-input-processors (instans continuouslyp))

;; (defgeneric run-input-processor (instans-input-processor))

;; (defgeneric instans-close-open-streams (instans))

;; (defgeneric process-query-input (instans-input-processor inputs &key graph ops))

;; (defgeneric initialize-reporting (instans reporting)
      
;; (defgeneric report-summary (instans))

;; (defgeneric report-sizes-headers (instans))

;; (defgeneric report-sizes (instans))

;; (defgeneric execute-rules (instans &optional policy))

;; (defgeneric output-quad-or-triple (instans s p o &optional g)

;; (defgeneric report-execution-status (instans &key stream))

;; (defun call-succ-nodes (func node token stack))

(defmethod instans::add-token :around ((node instans::node) token &optional stack)
  (declare (ignorable stack))
  (trace-wrap-call (list :add-token node token) :state-form (instans-trace-node-state node)))

(defmethod instans::add-alpha-token :around ((node instans::join-node) token &optional stack)
  (declare (ignorable stack))
  (trace-wrap-call (list :add-alpha-token node token) :state-form (instans-trace-node-state node)))

(defmethod instans::add-beta-token :around ((node instans::join-node) token &optional stack)
  (declare (ignorable stack))
  (trace-wrap-call (list :add-beta-token node token) :state-form (instans-trace-node-state node)))

(defmethod instans::remove-token :around ((node instans::node) token &optional stack)
  (declare (ignorable stack))
  (trace-wrap-call (list :remove-token node token) :state-form (instans-trace-node-state node)))

(defmethod instans::remove-alpha-token :around ((node instans::join-node) token &optional stack)
  (declare (ignorable stack))
  (trace-wrap-call (list :remove-alpha-token node token) :state-form (instans-trace-node-state node)))

(defmethod instans::remove-beta-token :around ((node instans::join-node) token &optional stack)
  (declare (ignorable stack))
  (trace-wrap-call (list :remove-beta-token node token) :state-form (instans-trace-node-state node)))

;; (defun rule-instance-queue-empty-p (queue))

;; (defun rule-instance-queue-add (queue node token))

;; (defun rule-instance-queue-remove (queue node token))

;; (defun rule-instance-queue-execute-instance (queue rule-instance))

;; (defun rule-instance-queue-execute-first (queue))

;; (defun rule-instance-queue-execute-snapshot (queue))

;; (defun rule-instance-queue-execute-count (queue))

;; (defun operation-report-p (instans kind))

;; (defgeneric rule-node-name-pretty (rule-node))

;; (defun node-token-bindings-for-reporting (node token))

;; (defun report-rule-op (queue op rule-instance &key stream))
 
;; (defun report-queue (queue &key stream))

;; (defgeneric execute-rule-node (node token))

;;; Group partition

;; (defgeneric aggregate-join-get-group (aggregate-join token))

;; (defun trace-rete ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instans::token-store-put :around ((node instans::token-store) token)
  (trace-wrap-call (list :token-store-put node token) :state-form (instans-trace-node-state node)))

(defmethod instans::token-store-put-if-missing :around ((node instans::token-store) token)
  (trace-wrap-call (list :token-store-put-if-missing node token) :state-form (instans-trace-node-state node)))

(defmethod instans::token-store-remove :around ((node instans::token-store) token)
  (trace-wrap-call (list :token-store-remove node token) :state-form (instans-trace-node-state node)))

(defmethod instans::token-store-remove-if-exists :around ((node instans::token-store) token)
  (trace-wrap-call (list :token-store-remove-if-exists node token) :state-form (instans-trace-node-state node)))

(defmethod instans::token-store-clear :around ((node instans::token-store))
  (trace-wrap-call (list :token-store-clear node) :state-form (instans-trace-node-state node)))

(defmethod instans::index-put-token :around ((this instans::token-index) key token)
  (let ((node (instans::token-index-node this)))
    (trace-wrap-call (list :index-put-token node key token) :state-form (instans-trace-node-state node))))

(defmethod instans::index-remove-token :around ((this instans::token-index) key token)
  (let ((node (instans::token-map-owner this)))
    (trace-wrap-call (list :index-remove-token node key token) :state-form (instans-trace-node-state node))))

(defmethod instans::token-map-put :around ((this instans::token-map) token value)
  (let ((node (instans::token-map-owner this)))
    (trace-wrap-call (list :token-map-put node token value) :state-form (instans-trace-node-state node))))

(defmethod instans::token-map-remove :around ((this instans::token-map) token)
  (let ((node (instans::token-map-owner this)))
    (trace-wrap-call (list :token-map-remove node token :state-form (instans-trace-node-state node)))))
