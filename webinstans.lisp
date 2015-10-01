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

(defun configurator-css ()
  (let ((border "1px solid #ccc"))
    (css-lite:css
     (("body")
      (:width "70%" :margin "0 auto" :font-family "sans-serif" :border-left border :border-right border :border-bottom border))
     (("h1")
      (:font-size "140%" :text-align "center"))
     (("h2")
      (:color "#000" :background-color "#cef" :margin "0 auto" :padding "4px 0"))
     (("#header")
      (:background-color "#cef" :padding "8px"))
     (("#header .logo")
      (:display "block" :margin "0 auto"))
     (("#header .strapline")
      (:display "block" :text-align "center" :font-size "80%" :font-style "italic")))))

(define-easy-handler (home-page :uri "/configure") ()
  (setf (content-type*) "text/html")
  (let ((output
	 (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
	   (:html :xmlns "http://www.w3.org/1999/xhtml"
		  :xml\:lang "en" 
		  :lang "en"
		  (:head 
		   (:meta :http-equiv "Content-Type" 
			  :content    "text/html;charset=utf-8")
		   (:title "WebINSTANS Configurator")
		   (:style :type "text/css" (cl-who:str (configurator-css)))
		   (:script :src "http://code.jquery.com/jquery-2.1.4.min.js")
		   (:script :src "http://layout.jquery-dev.com/lib/js/jquery.layout-latest.js")
;; <script>
;;     $(document).ready(function () {
;;         $('body').layout({ applyDefaultStyles: true });
;;     });
;; </script>
;; <script type='text/javascript'>at($(document), ready)(function () {
;;     return at($('block'), layout)({ applyDefaultStyles : true });
;; });
;; </script>
		   (:script (cl-who:str
						     ;; (ps ((@ ($ document) ready)
						     ;; 	  (lambda ()
						     ;; 	    ((@ ($ "input.btn") click)
						     ;; 	     (lambda ()
						     ;; 	       (block inner
						     ;; 		 (let ((text (chain ($ "input.txt") (val) (trim))))
						     ;; 		   (cond ((equal text "") 
						     ;; 			  ((@ ($ "#error") fade-in))
						     ;; 			  (return-from inner false)))))))))))))
						     ;; -->
						     ;; <script type='text/javascript'>at($(document), ready)(function () {
						     ;;     return at($('input.btn'), click)(function () {
						     ;;         var text = chain($('input.txt'), val(), trim());
						     ;;         if (text == '') {
						     ;;             at($('#error'), fadeIn)();
						     ;;             return false;
						     ;;         };
						     ;;     });
						     ;; });
						     ;; </script>
						     (ps ((parenscript:@ ($ document) ready)
						     	  (lambda ()
						     	    ((parenscript:@ ($ "body") layout)
							     (parenscript:create apply-default-styles t))))))))
		  (:body
		   (:div :class "ui-layout-center" "Center")
		   (:div :class "ui-layout-north" "North")
		   (:div :class "ui-layout-south" "South")
		   (:div :class "ui-layout-east" "East")
		   (:div :class "ui-layout-west" "West"))))
	  ))
    (with-open-file (ostr "output" :direction :output :if-exists :supersede)
      (format ostr "~%output:~%~A" output))
    output))
