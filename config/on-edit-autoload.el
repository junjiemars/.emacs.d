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


(defun edit-env->delete-trailing-whitespace ()
  "\\=`delete-trailing-whitespace\\=' before save."
  (when (apply #'derived-mode-p
               (*self-env-spec* :get :edit :delete-trailing-whitespace))
    (delete-trailing-whitespace)))


(defun self-edit-init! ()
  (let ((edit (*self-env-spec* :get :edit)))
		(when (self-spec-> edit :allowed)
			;; indent
			(let ((indent (self-spec-> edit :indent))
						(width (self-spec-> edit :tab-width)))
				(dolist* (x indent)
					(set (car x) (or (cdr x) width))))

			;; disable `indent-tabs-mode'
			(let ((modes (self-spec-> edit :disable-indent-tabs-mode)))
				(when (consp modes)
					(dolist* (x modes)
						(add-hook x #'edit-env->disable-indent-tabs-mode))))

      ;; default `tab-width' and `standard-indent'
      (let ((w (self-spec-> edit :tab-width)))
        (setq-default tab-width w
                      standard-indent w))

      ;; default `auto-save-default'
      (setq auto-save-default (self-spec-> edit :auto-save-default))

      ;; allow `narrow-to-region'
      (put 'narrow-to-region 'disabled
           (not (self-spec-> edit :narrow-to-region)))

      ;; `delete-trailing-whitespace' before save
      (add-hook 'before-save-hook #'edit-env->delete-trailing-whitespace))))



(make-thread* (self-edit-init!))


;; end of on-edit-autoload.el
