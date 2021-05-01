;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; ob-schemes.el
;;;;


(require 'ob)
(require 'comint)

(comment
 (declare-function run-picolisp "ext:inferior-picolisp" (cmd)))

(comment
 (defvar org-babel-tangle-lang-exts)) ;; Autoloaded

(comment
 ;; optionally define a file extension for this language
 (add-to-list 'org-babel-tangle-lang-exts '("picolisp" . "l")))

;;; interferes with settings in org-babel buffer?
;; optionally declare default header arguments for this language
;; (defvar org-babel-default-header-args:picolisp
;;   '((:colnames . "no"))
;;   "Default arguments for evaluating a picolisp source block.")

(comment
 (defvar org-babel-picolisp-eoe "org-babel-picolisp-eoe"
   "String to indicate that evaluation has completed."))

(comment
 (defcustom org-babel-picolisp-cmd "pil"
   "Name of command used to evaluate picolisp blocks."
   :group 'org-babel
   :version "24.1"
   :type 'string))

(comment
 (defun org-babel-expand-body:picolisp (body params)
   "Expand BODY according to PARAMS, return the expanded body."
   (let ((vars (org-babel--get-vars params))
         (print-level nil)
	       (print-length nil))
     (if (> (length vars) 0)
         (concat "(prog (let ("
                 (mapconcat
                  (lambda (var)
                    (format "%S '%S)"
                            (print (car var))
                            (print (cdr var))))
                  vars "\n      ")
                 " \n" body ") )")
       body))))

(defun org-babel-execute:scheme* (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing Scheme source code block, body=%s, params=%s" body params)
  (comment
   (let* (
	        ;; Name of the session or "none".
	        (session-name (cdr (assq :session params)))
	        ;; Set the session if the session variable is non-nil.
	        (session (org-babel-picolisp-initiate-session session-name))
	        ;; Either OUTPUT or VALUE which should behave as described above.
	        (result-params (cdr (assq :result-params params)))
	        ;; Expand the body with `org-babel-expand-body:picolisp'.
	        (full-body (org-babel-expand-body:picolisp body params))
          ;; Wrap body appropriately for the type of evaluation and results.
          (wrapped-body
           (cond
            ((or (member "code" result-params)
                 (member "pp" result-params))
             (format "(pretty (out \"/dev/null\" %s))" full-body))
            ((and (member "value" result-params) (not session))
             (format "(print (out \"/dev/null\" %s))" full-body))
            ((member "value" result-params)
             (format "(out \"/dev/null\" %s)" full-body))
            (t full-body)))
          (result
           (if (not (string= session-name "none"))
               ;; Session based evaluation.
               (mapconcat ;; <- joins the list back into a single string
                #'identity
                (butlast ;; <- remove the org-babel-picolisp-eoe line
                 (delq nil
                       (mapcar
                        (lambda (line)
                          (org-babel-chomp ;; Remove trailing newlines.
                           (when (> (length line) 0) ;; Remove empty lines.
                             (cond
                              ;; Remove leading "-> " from return values.
                              ((and (>= (length line) 3)
                                    (string= "-> " (substring line 0 3)))
                               (substring line 3))
                              ;; Remove trailing "-> <<return-value>>" on the
                              ;; last line of output.
                              ((and (member "output" result-params)
                                    (string-match-p "->" line))
                               (substring line 0 (string-match "->" line)))
                              (t line)
                              )
                             ;;(if (and (>= (length line) 3);Remove leading "<-"
                             ;;         (string= "-> " (substring line 0 3)))
                             ;;    (substring line 3)
                             ;;  line)
                             )))
                        ;; Returns a list of the output of each evaluated exp.
                        (org-babel-comint-with-output
                            (session org-babel-picolisp-eoe)
                          (insert wrapped-body) (comint-send-input)
                          (insert "'" org-babel-picolisp-eoe)
                          (comint-send-input)))))
                "\n")
             ;; external evaluation
             (let ((script-file (org-babel-temp-file "picolisp-script-")))
               (with-temp-file script-file
                 (insert (concat wrapped-body "(bye)")))
               (org-babel-eval
                (format "%s %s"
                        org-babel-picolisp-cmd
                        (org-babel-process-file-name script-file))
                "")))))
     (org-babel-result-cond result-params
       result
       (read result))))

  )

(comment
 (defun org-babel-picolisp-initiate-session (&optional session-name)
   "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
   (unless (string= session-name "none")
     (require 'inferior-picolisp)
     ;; provide a reasonable default session name
     (let ((session (or session-name "*inferior-picolisp*")))
       ;; check if we already have a live session by this name
       (if (org-babel-comint-buffer-livep session)
           (get-buffer session)
         (save-window-excursion
           (run-picolisp org-babel-picolisp-cmd)
           (rename-buffer session-name)
           (current-buffer)))))))


;;; Swtich to `org-babel-execute:scheme*'
;; (fset 'org-babel-execute:scheme #'org-babel-execute:scheme*)


(provide 'ob-schemes)



;;; eof
