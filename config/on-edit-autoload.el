;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-edit-autoload.el
;;;;


(defun edit-env->disable-indent-tabs-mode ()
  "Disable `indent-tabs-mode' in major mode."
  (set (make-local-variable 'indent-tabs-mode) nil))


(defun self-edit-init! ()
  (let ((edit (*self-env-spec* :get :edit)))
		(when (self-spec-> edit :allowed)
			(make-thread*
			 (lambda ()
				 ;; indent
				 (let ((indent (self-spec-> edit :indent))
							 (width (self-spec-> edit :tab-width)))
					 (dolist* (x indent)
						 (set (car x) (or (cdr x) width))))
				 ;; disable `indent-tabs-mode'
				 (let ((modes (self-spec-> edit :disable-indent-tabs-mode)))
					 (when (consp modes)
						 (dolist* (x modes)
							 (add-hook x #'edit-env->disable-indent-tabs-mode)))))))))


(self-edit-init!)

;; end of on-edit-autoload.el
