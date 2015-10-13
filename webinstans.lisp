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

(ps::defpsmacro stringify (&rest things)
  (if (and (= (length things) 1) (stringp (car things)))
      (car things)
      `(chain (list ,@things) (join ""))))

(define-easy-handler (home-page :uri "/configure") ()
  (setf (content-type*) "text/html")
  (let* ((split-command-cases (loop for case in instans::*instans-command-cases*
	 		        collect (instans::split-command-case-parameters case)))
	 (instans-options (loop for (name properties) in split-command-cases
				for usage-text = (format nil "~{~A~^ ~}" (getf properties :usage-texts))
				nconc (loop for option in (getf properties :expanded-options)
					    collect (list name option usage-text))))
	 (select-options (loop for (casename option usage-text) in instans-options
			       for option-name = (first option)
			       for option-type = (second option)
			       when (stringp option-name)
			       collect `(:option ,(format nil "~A~@[ [~A]~]" (subseq (first option) 2) (and (symbolp option-type) option-type)))))
	 (add-parameter (eval `(ps (defun add-parameter ()
				     (let* ((paramcount ((parenscript:@ ($ "#parameters") length)))
					    (name (concatenate 'string "param" paramcount)))
				       ((parenscript:@ ($ "#parameters") append)
					(ps::who-ps-html (:li (:select :name name :id name ,@select-options)))))))))
	 (head (cl-who::with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
		 (:head 
		  (:meta :charset "utf-8")
		  (:title "jQuery UI Tabs functionality")
		  (:link :href "http://code.jquery.com/ui/1.10.4/themes/smoothness/jquery-ui.css" :rel "stylesheet")
		  (:script :src "http://code.jquery.com/jquery-1.10.2.js")
		  (:script :src "http://code.jquery.com/ui/1.10.4/jquery-ui.js")
		  (:style (cl-who:str (configurator-css)))
		  (:script (cl-who:str
			    (ps ($ (lambda ()
				     ((parenscript:@ ($ "#tabs") tabs)
				      (parenscript:create :active 0)))))))
		  (:script (cl-who::str add-parameter))
		  )))
	 (body (cl-who::with-html-output-to-string (*standard-output* nil :indent t)
		 (:body
		  (:div :id "tabs"
			(:ul
			 (:li (:a :href "#configure-tab" "Configure"))
			 (:li (:a :href "#execute-tab" "Execute")))
			(:div :id "configure-tab"
			      (:form :action "#"
				     (:fieldset
				      (:ol :id "params"))))
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
      (format ostr "~%select-options:~%~S" select-options)
      (format ostr "~%add-parameter:~%~S" add-parameter)
      )
    output))
